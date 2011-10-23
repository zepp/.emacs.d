;; various auxiliary commands and functions -*- lexical-binding: t; -*-

;;;###autoload
(defun pavel/kill-process (&optional buffer)
  "Kills the BUFFER process. BUFFER is the current buffer by
default."

  (interactive
   (list (current-buffer)))

  (let ((proc (get-buffer-process buffer)))
    (if proc
        (interrupt-process proc)
      (user-error "Buffer '%s' has no running process" buffer))))

;;;###autoload
(defun pavel/start-presentation ()
  "it closes other windows, increases window face and enables text wrapping"
  (interactive)

  (delete-other-windows)
  (text-scale-set 2.2)
  (visual-fill-column-mode 1)
  (set-fill-column 30))

;;;###autoload
(defun pavel/upcase-buffer-uuids ()
  "upcases all UUID in the current buffer"
  (interactive)

  ;; https://steve-yegge.blogspot.com/2006/06/shiny-and-new-emacs-22.html

  (save-excursion
    (goto-char (point-min))

    (let ((uuid-re (string-join
                    '("[0-9a-f]\\{8\\}"
                      "[0-9a-f]\\{4\\}"
                      "[0-9a-f]\\{4\\}"
                      "[0-9a-f]\\{4\\}"
                      "[0-9a-f]\\{12\\}")
                    "-"))
          (case-fold-search nil))
      (while (re-search-forward uuid-re nil t)
        (replace-match (upcase (match-string 0)) t)))))

(provide 'pavel-aux)
