;; various compatibility commands and functions -*- lexical-binding: t; -*-

(defun u-sys-win-p ()
  (string= system-type "windows-nt"))

(defun u-sys-linux-p ()
  (string= system-type "gnu/linux"))

(defun u-sys-darwin-p ()
  (string= system-type "darwin"))

(defun u-emacs-30-p ()
  (string-match "30\\.[0-9]+" emacs-version))

(defun u-setup-features ()
  (let ((default-directory
         (expand-file-name "user-lisp/" user-emacs-directory)))
    (add-to-list 'load-path (directory-file-name default-directory))
    (loaddefs-generate default-directory ".user-lisp-autoloads.el")))

(when (u-emacs-30-p)
  (u-setup-features)
  (require '.user-lisp-autoloads))

(provide 'user-compat)
