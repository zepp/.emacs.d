;; my utility functions and crutches

(defun shell-jump ()
  "starts a shell in the current directory"

  (interactive)
  (let ((dir (directory-file-name default-directory)))
    (shell (concat (car (last (split-string dir "/")))
                   ":shell"))))

;;-------------------------------------------------------------------------------

(provide 'my-utils)
