(defun load-conf (file-sym conf-form &optional req)

  (defun do-load (conf)
    (let ((main  (expand-file-name
                  (format "conf.d/%s.el" conf) user-emacs-directory))
          (local (expand-file-name
                  (format "local.d/%s.el" conf) user-emacs-directory)))
      (if (file-regular-p local)
          `(progn (load ,main) (load ,local))
        `(load ,main))))

  (if (stringp conf-form)
      (eval-after-load file-sym (do-load conf-form))
    (eval-after-load file-sym conf-form))
  (when req
    (require file-sym nil t)))

;;-------------------------------------------------------------------------------

(defun shell-jump ()
  "opens the shell in the current directory. Opens new window if
prefix argument is set"

  (interactive)
  (let ((new-shell-buf-name
         (if (eq major-mode 'dired-mode)
             (concat (buffer-name) ":shell")
           (let* ((dir (directory-file-name
                        (if buffer-file-name
                            (file-name-directory buffer-file-name)
                          default-directory))))
             (concat (car (last (split-string dir "/")))
                     ":shell")))))
    (if current-prefix-arg
        (let ((split-height-threshold 0)
              (split-width-threshold nil))
          (shell new-shell-buf-name))
      (let (pop-up-windows
            (same-window-buffer-names
             (cons new-shell-buf-name same-window-buffer-names)))
        (shell new-shell-buf-name)))))

;;-------------------------------------------------------------------------------

(defun swap-buffers (&optional last)
  "Swaps the current and a last buffers"
  (interactive)
  (let ((buf (current-buffer))
        (frame (selected-frame)))
    (switch-to-buffer
     (if last
         (last-buffer buf t frame)
       (progn
         (other-buffer buf t frame)
         (bury-buffer buf))))))

(provide 'my-utils)
