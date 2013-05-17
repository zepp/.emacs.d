(defmacro gen-run-raise(name run-fun &optional buf-or-fun)
  (let* ((frm-name name)
         (frame-var-name (gensym))
         (do-a-frame `(progn
                        (message "making new frame for %s" ,name)
                        (select-frame-set-input-focus 
                         (make-frame '((window-system . x)
                                       (display . ,(getenv "DISPLAY"))
                                       (name . ,frm-name))))
                        (funcall ,run-fun))))
    (if buf-or-fun
        `(let ((,frame-var-name (find-if (lambda (frame)
                                           (string= ,frm-name
                                                    (frame-parameter frame 'name)))
                                         (frame-list))))
           (if ,frame-var-name
               (progn
                 (select-frame-set-input-focus ,frame-var-name)
                 (if (functionp ,buf-or-fun)
                     (funcall ,buf-or-fun)
                   (switch-to-buffer ,buf-or-fun)))
             ,do-a-frame))
      do-a-frame)))

(defun wm-run-jabber()
  (when (boundp 'jabber-roster-buffer)
    (gen-run-raise "jabber" #'jabber-connect-with-secrets)))

(defun wm-run-wl()
  (when (boundp 'wl-folder-buffer-name)
    (gen-run-raise "wl" #'wl)))

(defun wm-run-mpc()
  (when (boundp 'mpc-status)
    (gen-run-raise "mpc" #'mpc)))

(defun wm-run-agenda()
  (when (boundp 'org-agenda-files)
    (gen-run-raise "agenda" #'org-agenda)))
