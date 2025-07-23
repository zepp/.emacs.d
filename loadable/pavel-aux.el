;; various auxiliary commands and functions

;;;###autoload
(defun pavel/install-ts-grammars ()
  "installs tree-sitter language grammars"

  (interactive)
  (let ((ts-path (expand-file-name "tree-sitter" user-emacs-directory))
        (langs-path (tree-sitter-langs--bin-dir)))
    (when (file-directory-p ts-path)
      (rename-file ts-path (concat ts-path ".old")))
    (make-directory ts-path)
    (dolist (file (directory-files langs-path nil "\\.\\(so\\|dll\\|dylib\\)$"))
      (copy-file (expand-file-name file langs-path)
                 (expand-file-name (concat "libtree-sitter-" file) ts-path)
                 t
                 t)
      (message "%s is installed" file))))

;;;###autoload
(defun pavel/double-q-marks ()
  "if region is active then it wraps marked text with double angle
quotation marks otherwise just inserts it"
  (interactive)

  (if (use-region-p)
        (let* ((start (region-beginning))
               (end (region-end))
               (text (buffer-substring start end)))
          (delete-region start end)
	  (insert (format "«%s»" text)))

      (insert "«»")
      (backward-char 1)))

;;;###autoload
(defun pavel/insert-dash (arg)
  "inserts em dash, en dash or hyphen character according to numeric
prefix argument"
  (interactive "p")

  (insert-char
   (cond ((or (eq 3 current-prefix-arg)
              (not current-prefix-arg))
          (char-from-name "EM DASH"))
         ((eq 2 current-prefix-arg)
          (char-from-name "EN DASH"))
         (t (char-from-name "HYPHEN")))
   1 t))

;;;###autoload
(defun pavel/eshell-buf-name (&optional directory)
  "it provides eshell buffer name that includes directory name. Naming is
simillar to the project one"

  (let* ((dir-file-name (abbreviate-file-name
                         (directory-file-name
                          (or directory default-directory))))
         (name (car
                (reverse
                 (file-name-split dir-file-name)))))
    (format (if (string= "" name)
                "*eshell*"
              "*%s-eshell*")
            name)))

;;;###autoload
(defun pavel/setup-org-directory (directory)
  "it setups Org-mode's root directory and agenda files"
  (interactive "D")

  (setq
   org-directory directory
   org-agenda-files `(,directory)
   org-default-notes-file (expand-file-name "default.org" directory)
   org-archive-location (expand-file-name "archive.org::" directory)))

;;;###autoload
(defun pavel/start-presentation ()
  "it closes other windows, increases window face and enables text wrapping"
  (interactive)

  (delete-other-windows)
  (text-scale-set 2.2)
  (visual-fill-column-mode 1)
  (set-fill-column 30))

(provide 'pavel-aux)
