;; various compatibility commands and functions -*- lexical-binding: t; -*-

(defun user/sys-win-p ()
  (string= system-type "windows-nt"))

(defun user/sys-linux-p ()
  (string= system-type "gnu/linux"))

(defun user/sys-darwin-p ()
  (string= system-type "darwin"))

(provide 'user-compat)
