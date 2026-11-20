;; various compatibility commands and functions -*- lexical-binding: t; -*-

;;;###autoload
(defun user/sys-win-p ()
  (string= system-type "windows-nt"))

;;;###autoload
(defun user/sys-linux-p ()
  (string= system-type "gnu/linux"))

;;;###autoload
(defun user/sys-darwin-p ()
  (string= system-type "darwin"))

;;;###autoload
(defun pavel/list-dicts(directory)
  "builds dictionary path alist for hunspell"

  (let ((root (expand-file-name directory)))
    (mapcar #'(lambda(file)
                (list (substring file 0 -4)
                      (expand-file-name file root)))
            (directory-files
             root
             nil
             "[[:lower:]]\\{2\\}_[[:upper:]]\\{2\\}\\.dic"))))

(provide 'pavel-compat)
