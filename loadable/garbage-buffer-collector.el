;;; garbage-buffer-collector.el --- collect unused buffers and release memory -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Pavel Sokolov
;; All rights reserved.

;; Author: Pavel Sokolov <pavel@warp-link.ru>
;; Maintainer: Pavel Sokolov <pavel@warp-link.ru>
;; Created: 2026

;; This file is NOT part of GNU Emacs.

;;;###autoload
(define-minor-mode persistent-buffer-mode
  "Minor mode to protect a buffer from a garbage collection"
  ;; The indicator for the mode line.
  :lighter " ∞")

(defgroup garbage-buffer-collector nil "Main group")

(defcustom garbage-buffer-collection-delay (* 3 60)
  "Number of minutes to delay a garbage buffer collection after
Emacs idle."
  :group 'garbage-buffer-collector
  :type 'natnum)

(defcustom garbage-buffer-query '()
  "Query to form a candidate list of buffers to be collected and
released. Take a look at `buffer-match-p' for details. All
buffers are considered by default."
  :group 'garbage-buffer-collector
  :type 'sexp)

(defcustom garbage-buffer-memory-threshold (* 32 1024)
  "Total amount of allocated buffer memory that is required to start
a garbage collection."
  :group 'garbage-buffer-collector
  :type 'natnum)

(defcustom garbage-buffer-collect-ephemeral nil
  "If non-nil then ephemeral buffers are collectible. Buffers that
are ephemeral and generally uninteresting to the user have names
starting with a space."
  :group 'garbage-buffer-collector
  :type 'boolean)

(defcustom garbage-buffer-debug nil
  "If non-nil then additional information is printed."
  :group 'garbage-buffer-collector
  :type 'boolean)

(defcustom garbage-buffer-max-name-width 20
  "Maximal buffer name width in debug output"
  :group 'garbage-buffer-collector
  :type 'natnum)

(defconst garbage-buffer-collector-timer
  (let ((timer (timer-create))
        (seconds (* garbage-buffer-collection-delay 60)))
    (timer-set-function timer #'garbage-buffer-collect)
    (timer-set-idle-time timer seconds)
    timer)
  "Timer to schedule a garbage buffer collection. It is activated by
`garbage-buffer-schedule'")

(defvar garbage-buffer-last-base-point nil
  "base point to measure buffer weight")

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
  (unless garbage-buffer-last-base-point
    (setq garbage-buffer-last-base-point (current-time))))

(defun garbage-buffer-schedule ()
  "Schedules a garbage buffer collection."
  (when-let* ((timer garbage-buffer-collector-timer)
              (p (not (memq timer timer-idle-list))))
    (timer-activate-when-idle timer)))

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
          (width garbage-buffer-max-name-width)
          (alist))
      (push (cons 'buffer buffer) alist)
      (push (cons 'name (truncate-string-to-width (buffer-name) width nil nil t))
            alist)
      (when (and collectible (not persistent))
        (push (cons 'collectible t) alist))
      (when (> buffer-display-count 0)
        (push (cons 'display-count buffer-display-count) alist))
      (when buffer-display-time
        (push (cons 'display-time (time-convert buffer-display-time 'integer))
              alist))
      (when-let* ((ticks (buffer-chars-modified-tick))
                  (p (> ticks 0)))
        (push (cons 'chars-modified-tick ticks) alist))
      (let ((size (buffer-size)))
        (push (cons 'size size) alist)
        (push (cons 'weight (float size)) alist))
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

(defun garbage-buffer-adjust-weight (buffers property factor-addendum &optional max-value)
  "Adjusts weight according to a buffer PROPERTY. The more PROPERTY
value the fewer weight since weight is simply multiplied by a
calculated factor. Basic factor is aproximatley in range of 0 to
1 and shifted by FACTOR-ADDENDUM."

  (cl-assert (length> buffers 0))
  (let* ((min (garbage-buffer-find-by-property property #'< buffers))
         (max (if max-value
                  max-value
                (1+ (garbage-buffer-find-by-property property #'> buffers))))
         (delta (- max min)))
    (seq-do #'(lambda (buf-info)
                (let* ((weight (assq 'weight buf-info))
                       (value (alist-get property buf-info min))
                       (factor (/ (- max value) (float delta))))
                  (setcdr weight (* (cdr weight) (+ factor factor-addendum)))))
            buffers)
    (cons min (1- delta))))

(defun garbage-buffer-divide (buffers time)
  "Divides BUFFERS into groups according to a display time. First
group contains buffers with `buffer-display-time' after TIME,
next one before TIME and last one contains non-displayed buffers."
  (let* ((non-displayed (seq-remove (apply-partially #'alist-get 'display-time)
                                    buffers))
         (displayed (seq-sort
                     #'(lambda (a b)
                         (< (alist-get 'display-time a)
                            (alist-get 'display-time b)))
                     (seq-difference buffers non-displayed)))
         (groups (seq-group-by
                  #'(lambda (buf-info)
                      (> (alist-get 'display-time buf-info) time))
                  displayed)))
    (list
     (cdr (nth 1 groups))
     (nreverse (cdr (nth 0 groups)))
     non-displayed)))

(defun garbage-buffer-sort (buffers time)
  "Measures buffer weight and sorts BUFFERS list accoding to it."
  (let* ((last-time (time-convert garbage-buffer-last-base-point 'integer))
         (groups (garbage-buffer-divide buffers last-time))
         (utilized (nth 0 groups))
         (unutilized (nth 1 groups))
         (non-displayed (nth 2 groups)))
    (garbage-buffer-adjust-weight buffers 'chars-modified-tick 1)
    (seq-do #'(lambda (buf-info)
                (let ((weight (assq 'weight buf-info)))
                  (setcdr weight (* (cdr weight) 2))))
            non-displayed)
    (garbage-buffer-adjust-weight (append utilized unutilized) 'display-count 1)
    (when utilized
      (garbage-buffer-adjust-weight utilized 'display-time 1 time))
    (when unutilized
      (garbage-buffer-adjust-weight unutilized 'display-time 2 last-time))
    (seq-sort #'(lambda (a b)
                  (> (alist-get 'weight a)
                     (alist-get 'weight b)))
              buffers)))

(defun garbage-buffer-reduce (buffers time)
  "Reduces BUFFERS list if one's total memory size is greater then
`garbage-buffer-memory-threshold'. `garbage-buffer-sort' decides
which buffers should be released last."

  (let* ((threshold garbage-buffer-memory-threshold)
         (buffers (garbage-buffer-sort buffers (time-convert time 'integer)))
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

(defun garbage-buffer-replace-indirect-bufs (buffers)
  "Replaces indirect buffers with base buffers in BUFFERS list and
returns a list of unique entries."

  (seq-uniq
   (seq-map
    #'(lambda (buf)
        (or (buffer-base-buffer buf)
            buf))
    buffers)))

(defun garbage-buffer-print (buffers)
  "Print BUFFERS names and amount of allocated memory."
  (if garbage-buffer-debug
      (let* ((width (+ 5 garbage-buffer-max-name-width))
             (format (concat "%" (number-to-string width) "s" "%10d %10.1f %5.1f")))
        (dolist (buf buffers)
          (let* ((size (alist-get 'size buf))
                 (weight (alist-get 'weight buf))
                 (ratio (/ weight size)))
            (message format (alist-get 'name buf) size weight ratio))))
    (message "garbage buffers: %s "
             (string-join
              (seq-map (apply-partially #'alist-get 'name) buffers)
              " ")))
  (let ((bytes (garbage-buffer-mem-size buffers)))
    (message "total size: %s bytes" bytes)))

;;;###autoload
(defun garbage-buffer-collect (&optional no-reduce)
  "Runs a garbage buffer collection. List of collectible buffers is
reduced by `garbage-buffer-reduce' if NO-REDUCE argument is nil."
  (interactive)
  (when-let* ((time (garbage-buffer-emacs-last-active))
              (buffers (mapcar #'garbage-buffer-get-info
                               (garbage-buffer-replace-indirect-bufs
                                (if garbage-buffer-query
                                    (match-buffers garbage-buffer-query)
                                  (buffer-list)))))
              (collectible (seq-filter (apply-partially #'alist-get 'collectible) buffers)))
    (message "last active time - %s" (format-time-string "%F %R" time))
    (message "%i/%i buffers are collectible" (length collectible) (length buffers))
    (when-let* ((reduced (if no-reduce
                             collectible
                           (garbage-buffer-reduce collectible time))))
      (mapcar #'kill-buffer (mapcar (apply-partially #'alist-get 'buffer) reduced))
      (garbage-buffer-print reduced)
      (cancel-timer garbage-buffer-collector-timer)
      (setq garbage-buffer-last-base-point time))))

(provide 'garbage-buffer-collector)
