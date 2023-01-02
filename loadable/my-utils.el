(cl-defun load-package (name &key required after-load config options)

  (when (or after-load config options)
    (lexical-let ((closure-after-load after-load)
                  (closure-config config)
                  (closure-options options))
      (eval-after-load name
        #'(lambda()
            (when closure-after-load
              (mapcar #'eval closure-after-load))

            (when closure-config
              (let ((default (expand-file-name
                              (format "conf.d/%s.el" closure-config) user-emacs-directory))
                    (local (expand-file-name
                            (format "local.d/%s.el" closure-config) user-emacs-directory)))

                (load default)
                (when (file-regular-p local)
                  (load local))))

            (when closure-options
              (mapcar #'(lambda (option) (setf (car option) (cdr option))) closure-options))))))

  (when required
    (require name nil t)))

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

(provide 'my-utils)
