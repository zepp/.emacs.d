(defun run-in-frame(name fun)
  (message "making new frame for %s" name)
  (select-frame (make-frame `((window-system . x)
                              (display . ,(getenv "DISPLAY"))
                              (name . ,name))))
  (funcall fun))

(defun wm-run-wl()
  (when (boundp 'wl-folder-buffer-name)
    (run-in-frame "wl" #'wl)))

(defun wm-run-agenda()
  (when (boundp 'org-agenda-files)
    (run-in-frame "agenda" #'org-agenda)))
