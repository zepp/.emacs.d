;; various compatibility commands and functions -*- lexical-binding: t; -*-

(defun user/sys-win-p ()
  (string= system-type "windows-nt"))

(defun user/sys-linux-p ()
  (string= system-type "gnu/linux"))

(defun user/sys-darwin-p ()
  (string= system-type "darwin"))

(defun emacs-version-30-p ()
  (string-match "30\\.[0-9]+" emacs-version))

(defun user/setup-features (&optional dir)
  (let* ((default-directory
          (expand-file-name (or dir "user-lisp/") user-emacs-directory))
         (paths (delete ".." (directory-files default-directory)))
         (dirs (mapcar #'expand-file-name (seq-filter #'file-directory-p paths))))
    (normal-top-level-add-to-load-path dirs)
    (loaddefs-generate dirs ".user-lisp-autoloads.el")))

(when (emacs-version-30-p)
  (user/setup-features)
  (require '.user-lisp-autoloads))

(provide 'user-compat)
