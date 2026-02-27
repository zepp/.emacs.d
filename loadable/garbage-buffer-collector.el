;;; garbage-buffer-collector.el --- collect unused buffers and release memory -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Pavel Sokolov
;; All rights reserved.

;; Author: Pavel Sokolov <pavel.zepp@gmail.com>
;; Maintainer: Pavel Sokolov <pavel.zepp@gmail.com>
;; Created: 2025

;; This file is NOT part of GNU Emacs.

;;;###autoload
(define-minor-mode persistent-buffer-mode
  "Minor mode to protect a buffer from a garbage collection"
  ;; The indicator for the mode line.
  :lighter " ∞")

(defgroup garbage-buffer-collector nil "Main group")

(defcustom garbage-buffer-collection-delay (* 3 60)
  "Number of minutes to delay a garbage buffer collection."
  :group 'garbage-buffer-collector
  :type 'natnum)

(defcustom garbage-buffer-query '()
  "Query to form a list of garbage buffers to be collected and
released. Take a look at `buffer-match-p' for details."
  :group 'garbage-buffer-collector
  :type 'sexp)

(defcustom garbage-buffer-memory-threshold (* 32 1024)
  "total amount of allocated buffer memory that is required to start
a garbage collection"
  :group 'garbage-buffer-collector
  :type 'natnum)

(defcustom garbage-buffer-collect-ephemeral nil
  "If non-nill then ephemeral buffers are collectible. Buffers that
are ephemeral and generally uninteresting to the user have names
starting with a space."
  :group 'garbage-buffer-collector
  :type 'boolean)

(defconst garbage-buffer-collector-timer
  (let ((timer (timer-create))
        (seconds (* garbage-buffer-collection-delay 60)))
    (timer-set-function timer #'garbage-buffer-collect)
    (timer-set-idle-time timer seconds)
    timer)
  "Timer to schedule a garbage buffer collection. It is activated by
`garbage-buffer-schedule'")

(defvar garbage-buffer-last-base-point nil
  "base point to measure buffer metrics")

(defun garbage-buffer-collector-set-delay (hours &optional minutes)
  "Updates a delay to run a garbage buffer collection and
`garbage-buffer-collection-delay' special variable."
  (cl-assert (and (natnump hours)
                  (if minutes (natnump minutes) t)))
  (let ((timer garbage-buffer-collector-timer)
        (minutes (+ (* hours 60) (or minutes 0))))
    (timer-set-idle-time timer (* minutes 60))
    (setq garbage-buffer-collection-delay minutes)))

;;;###autoload
(defun garbage-buffer-collector-init ()
  "Initializes the garbage buffer collector to release unused
buffers after `garbage-buffer-collection-delay' when Emacs is
idle."
  (interactive)
  (add-hook 'buffer-list-update-hook
            #'garbage-buffer-schedule)
  (garbage-buffer-schedule)
  (setq garbage-buffer-last-base-point (current-time)))

(defun garbage-buffer-schedule ()
  "Schedules a garbage buffer collection."
  (when-let* ((timer garbage-buffer-collector-timer)
              (p (not (memq timer timer-idle-list))))
    (timer-activate-when-idle timer)))

(defun garbage-buffer-diff-time (a b)
  (abs (time-convert (time-subtract a b) 'integer)))

(defun garbage-buffer-collectible-p (buffer)
  "Checks that BUFFER is collectible and can be released. Modified
buffers, buffers running process or buffers displayed in a window
are not not collectible."
  (and (buffer-live-p buffer)
       (not (or (and (buffer-file-name buffer) (buffer-modified-p buffer))
                (get-buffer-process buffer)
                (get-buffer-window buffer t)))
       (if (string-match-p "^\\s-" (buffer-name buffer))
           garbage-buffer-collect-ephemeral
         t)))

(defun garbage-buffer-get-info (buffer)
  "Creates alist that holds BUFFER properties to consider one
collectible and measure one's weight to optimize gc."
  (with-current-buffer buffer
    (let ((collectible (garbage-buffer-collectible-p buffer))
          (persistent (memq 'persistent-buffer-mode local-minor-modes))
          (alist))
      (push (cons 'buffer buffer) alist)
      (push (cons 'name (buffer-name)) alist)
      (when (and collectible (not persistent))
        (push (cons 'collectible t) alist))
      ;; 0 makes `garbage-buffer-adjust-weight-by-count' suboptimal
      (when (> buffer-display-count 0)
        (push (cons 'display-count buffer-display-count) alist))
      (push (cons 'last-activity-time (or buffer-display-time
                                          before-init-time))
            alist)
      (push (cons 'size (buffer-size)) alist)
      (push (cons 'weight (buffer-size)) alist)
      (nreverse alist))))

(defmacro garbage-buffer-mem-size (buffers)
  `(apply #'+ (mapcar (apply-partially #'alist-get 'size) ,buffers)))

(defun garbage-buffer-find-by-property (symbol comparator buffers)
  (cl-assert (and (symbolp symbol)
                  (functionp comparator)))
  (seq-reduce
   #'(lambda (accum buf-info)
       (let ((value (alist-get symbol buf-info)))
         (if (and value (or (null accum) (funcall comparator value accum)))
             value
           accum)))
   (cdr buffers)
   (alist-get symbol (car buffers))))

(defun garbage-buffer-adjust-weight-by-time (buffers time-base-point &optional addendum)
  "Adjusts weight according to a buffer last activity time. ADDENDUM
is a value added to a calculated factor."

  (cl-assert (length> buffers 0))
  (let* ((min (garbage-buffer-find-by-property 'last-activity-time
                                               #'time-less-p buffers))
         (delta (garbage-buffer-diff-time time-base-point min))
         (addendum (or addendum 0)))
    (seq-do #'(lambda (buf-info)
                (let* ((weight (assq 'weight buf-info))
                       (time (alist-get 'last-activity-time buf-info))
                       (factor (/ (garbage-buffer-diff-time time-base-point time)
                                  (float delta))))
                  (setcdr weight (* (cdr weight) (+ factor addendum)))))
            buffers)
    (cons min delta)))

(defun garbage-buffer-adjust-weight-by-count (buffers)
  "Adjusts weight according to a buffer display count. Weight is
multiplied by a calculated factor. Factor value is in range from
1 to ~2. Weight of buffer that has not been displayed at all or
has minimal display count is multiplied by ~2."

  (cl-assert (length> buffers 0))
  (let* ((min (garbage-buffer-find-by-property 'display-count #'< buffers))
         (max (1+ (garbage-buffer-find-by-property 'display-count #'> buffers)))
         (delta (- max min)))
    (seq-do #'(lambda (buf-info)
                (let* ((weight (assq 'weight buf-info))
                       (count (alist-get 'display-count buf-info min))
                       (factor (/ (- max count) (float delta))))
                  (setcdr weight (* (cdr weight) (1+ factor)))))
            buffers)
    (cons min (1- delta))))

(defun garbage-buffer-divide (buffers time)
  "Divides BUFFERS into groups. Former group contains buffers with
`last-activity-time' after TIME, latter before TIME."
  (let* ((buffers (seq-sort
                   #'(lambda (a b)
                       (time-less-p (alist-get 'last-activity-time a)
                                    (alist-get 'last-activity-time b)))
                   buffers))
         (groups (seq-group-by
                  #'(lambda (buf-info)
                      (time-less-p (alist-get 'last-activity-time buf-info)
                                   time))
                  buffers)))
    (cons (cdr (nth 1 groups))
          (cdr (nth 0 groups)))))

(defun garbage-buffer-sort (buffers time)
  "Sorts BUFFERS list accoding to a buffer weight."
  (let* ((last-time garbage-buffer-last-base-point)
         (groups (garbage-buffer-divide buffers last-time))
         (utilized (car groups))
         (unused (cdr groups)))
    (garbage-buffer-adjust-weight-by-count buffers)
    (when utilized
      (garbage-buffer-adjust-weight-by-time utilized time 1))
    (when unused
      (garbage-buffer-adjust-weight-by-time unused last-time 2))
    (seq-sort #'(lambda (a b)
                  (> (alist-get 'weight a)
                     (alist-get 'weight b)))
              buffers)))

(defun garbage-buffer-reduce (buffers time)
  "Reduces BUFFERS list if one's total memory size is greater then
`garbage-buffer-memory-threshold'. `garbage-buffer-sort' decides
which buffers should be released last."

  (let* ((threshold garbage-buffer-memory-threshold)
         (buffers (garbage-buffer-sort buffers time))
         (total-size (garbage-buffer-mem-size buffers)))
    (when (> total-size threshold)
      (let ((release-at-least (- total-size threshold)))
        (seq-reduce
         #'(lambda (accum buf)
             (when (< (garbage-buffer-mem-size accum) release-at-least)
               (push buf accum))
             accum)
         buffers
         nil)))))

(defun garbage-buffer-emacs-last-active ()
  "Returns a last moment when Emacs was active"
  (let ((idle-time (current-idle-time))
        (current-time (current-time)))
    (if idle-time
        (time-subtract current-time idle-time)
      current-time)))

(defun garbage-buffer-list-to-string (buffers)
  "Joins BUFFERS names to a string for a debug purposes."
  (string-join
   (seq-map #'(lambda (buf)
                (truncate-string-to-width (alist-get 'name buf) 20 nil nil t))
            buffers)
   " "))

;;;###autoload
(defun garbage-buffer-collect (&optional no-reduce)
  "Runs a garbage buffer collection. List of collected buffers is
reduced by `garbage-buffer-reduce' if NO-REDUCE argument is nil."
  (interactive)
  (when-let* ((time (garbage-buffer-emacs-last-active))
              (buffers (mapcar #'garbage-buffer-get-info
                               (if garbage-buffer-query
                                   (match-buffers garbage-buffer-query)
                                 (buffer-list))))
              (collectible (seq-filter (apply-partially #'alist-get 'collectible) buffers)))
    (message "last active time - %s" (format-time-string "%F %R" time))
    (message "%i/%i buffers are collectible" (length collectible) (length buffers))
    (when-let* ((reduced (if no-reduce
                             collectible
                           (garbage-buffer-reduce collectible time)))
                (mem-to-release (garbage-buffer-mem-size reduced))
                (names (garbage-buffer-list-to-string reduced)))
      (mapcar #'kill-buffer (mapcar (apply-partially #'alist-get 'buffer) reduced))
      (message "released buffers: %s" names)
      (message "%s bytes are realsed" mem-to-release)
      (cancel-timer garbage-buffer-collector-timer)
      (setq garbage-buffer-last-base-point time))))

(provide 'garbage-buffer-collector)
