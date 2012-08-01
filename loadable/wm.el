(defmacro wm-generic-raise(name buf-or-fun start-fun)
  (let* ((frm-name (concat "emacs-" name))
	 (frm-pred `(lambda (frame)
			(string= ,frm-name (frame-parameter frame 'name))))
	 (frame-var-name (gensym)))
    `(let ((,frame-var-name (find-if ,frm-pred (frame-list))))
       (if ,frame-var-name
	   (progn
	     (select-frame-set-input-focus ,frame-var-name)
	     (if (functionp ,buf-or-fun)
		 (funcall ,buf-or-fun)
	       (switch-to-buffer ,buf-or-fun)))
	 (progn
	   (message "making new frame for %s" ,name)
	   (select-frame-set-input-focus (make-frame '((window-system . x)
						       (name . ,frm-name)
						       (auto-raise . t))))
	   (funcall ,start-fun))))))

(defun wm-raise-jabber()
  (when (boundp 'jabber-roster-buffer)
    (wm-generic-raise "jabber" #'jabber-switch-to-roster-buffer
		      #'jabber-connect-with-secrets)))


(defun wm-raise-mail()
  (when (boundp 'wl-folder-buffer-name)
    (wm-generic-raise "mail" #'(lambda ()
				 (switch-to-buffer wl-folder-buffer-name)
				 (wl-folder-check-all)) 
		      #'wl)))
