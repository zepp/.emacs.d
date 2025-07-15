;; various auxiliary commands and utilities

;;;###autoload
(defun pavel/start-presentation ()
  "it closes other windows, increases window face and enables text wrapping"
  (interactive)

  (delete-other-windows)
  (text-scale-set 2.2)
  (visual-fill-column-mode)
  (set-fill-column 30))

(provide 'pavel-commands)
