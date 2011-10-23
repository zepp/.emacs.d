;; various compatibility commands and functions -*- lexical-binding: t; -*-

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

;;;###autoload
(defun pavel/fix-find-regexp (list)
  (mapcar #'(lambda(entry)
              (if (string-match "\\^find" (car entry))
                  (list (string-replace "^find" "find\\.exe" (car entry))
                        (cadr entry))
                entry))
          list))

;;;###autoload
(defun pavel/toggle-window-dedicated (arg)
  "Toggles window dedication in the selected window."

  (interactive "P")

  (let* ((window (selected-window))
         (flag (if arg (/= arg 0) (not (window-dedicated-p window)))))
    (set-window-dedicated-p window flag)
    (message "set window dedicated - %s" flag)))

(provide 'pavel-compat)
