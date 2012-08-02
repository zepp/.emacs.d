(defmacro wm-generic-raise(name start-fun &optional buf-or-fun)
  (let* ((frm-name (concat "emacs-" name))
	 (frm-pred `(lambda (frame)
		      (string= ,frm-name (frame-parameter frame 'name))))
	 (frame-var-name (gensym)))
    `(let ((,frame-var-name (find-if ,frm-pred (frame-list))))
       (if ,frame-var-name
	   ,(when buf-or-fun
	      `(progn
		 (select-frame-set-input-focus ,frame-var-name)
		 (if (functionp ,buf-or-fun)
		     (funcall ,buf-or-fun)
		   (switch-to-buffer ,buf-or-fun))))
	 (progn
	   (message "making new frame for %s" ,name)
	   (select-frame-set-input-focus (make-frame '((window-system . x)
						       (name . ,frm-name))))
	   (funcall ,start-fun))))))

(defun wm-raise-jabber()
  (when (boundp 'jabber-roster-buffer)
    (wm-generic-raise "jabber" #'jabber-connect-with-secrets)))

(defun wm-raise-mail()
  (when (boundp 'wl-folder-buffer-name)
    (wm-generic-raise "mail" #'wl)))

