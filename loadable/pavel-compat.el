;; various compatibility commands and functions

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

(defun pavel/fix-find-regexp (list)
  (mapcar #'(lambda(entry)
              (if (string-match "\\^find" (car entry))
                  (list (string-replace "^find" "find\\.exe" (car entry))
                        (cadr entry))
                entry))
          list))

(provide 'pavel-compat)
