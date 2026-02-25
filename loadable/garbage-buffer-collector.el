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

(defun garbage-buffer-persistent-p (buffer)
  "Checks that `persistent-buffer-mode' is enabled in BUFFER."
  (with-current-buffer buffer
    (memq 'persistent-buffer-mode local-minor-modes)))

(defun garbage-buffer-collectible-p (buffer)
  "Checks that BUFFER is collectible and can be released. Modified
buffers, buffers running process or buffers displayed in a window
are not not collectible."
  (and (buffer-live-p buffer)
       (not (or (garbage-buffer-persistent-p buffer)
                (and (buffer-file-name buffer) (buffer-modified-p buffer))
                (get-buffer-process buffer)
                (get-buffer-window buffer t)))))

(defmacro garbage-buffer-mem-size (buffers)
  `(apply #'+ (mapcar #'buffer-size ,buffers)))

(defun garbage-buffer-sort (buffers time)
  "sorts BUFFERS list accoding to a buffer weight."
  (seq-sort #'(lambda (a b)
                (> (buffer-size a)
                   (buffer-size b)))
            buffers))

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
                (truncate-string-to-width (buffer-name buf) 20 nil nil t))
            buffers)
   " "))

;;;###autoload
(defun garbage-buffer-collect (&optional no-reduce)
  "Runs a garbage buffer collection. List of collected buffers is
reduced by `garbage-buffer-reduce' if NO-REDUCE argument is nil."
  (interactive)
  (when-let* ((time (garbage-buffer-emacs-last-active))
              (buffers (if garbage-buffer-query
                           (match-buffers garbage-buffer-query)
                         (buffer-list)))
              (collectible (seq-filter #'garbage-buffer-collectible-p buffers)))
    (message "last active time - %s" (format-time-string "%F %R" time))
    (message "%i/%i buffers are collectible" (length collectible) (length buffers))
    (when-let* ((reduced (if no-reduce
                             collectible
                           (garbage-buffer-reduce collectible time)))
                (mem-to-release (garbage-buffer-mem-size reduced))
                (names (garbage-buffer-list-to-string reduced)))
      (mapcar #'kill-buffer reduced)
      (message "released buffers: %s" names)
      (message "%s bytes are realsed" mem-to-release)
      (cancel-timer garbage-buffer-collector-timer)
      (setq garbage-buffer-last-base-point time))))

(provide 'garbage-buffer-collector)
