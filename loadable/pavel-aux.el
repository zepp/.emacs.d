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

(defvar pavel/quotation-marks-alist
  '((primary . ((start . "«")
                (end . "»")))
    (secondary . ((start . "„")
                  (end . "“")))
    (plain . ((start . "\"")
              (end . "\"")))))

(defun pavel/build-q-regexp (type)
  (let* ((marks (cdr (assoc type pavel/quotation-marks-alist)))
         (start (cdr (assoc 'start marks)))
         (end (cdr (assoc 'end marks))))
    (format "^%s[^%s]+%s$" start end end)))

(defun pavel/quote (type &optional text)
  (let* ((marks (cdr (assoc type pavel/quotation-marks-alist)))
         (start (cdr (assoc 'start marks)))
         (end (cdr (assoc 'end marks))))
    (format "%s%s%s" start (or text "") end)))

(defun pavel/check-quotes (text)
  (cond
   ((string-match (pavel/build-q-regexp 'primary) text)
    'primary)
   ((string-match (pavel/build-q-regexp 'secondary) text)
    'secondary)
   ((string-match (pavel/build-q-regexp 'plain) text)
    'plain)))

;;;###autoload
(defun pavel/smart-q-marks (arg)
  "if region is active then text is wrapped with quotation marks
otherwise ones are just inserted"
  (interactive "p")

  (let ((new-type (if (or (not arg) (eq arg 1))
                      'primary
                    'secondary)))

    (if (use-region-p)
        (let* ((start (region-beginning))
               (end (region-end))
               (text (buffer-substring start end))
               (type (pavel/check-quotes text)))
          (when (or
                 (and (eq type 'primary)
                      (eq new-type 'secondary))
                 (and (eq type 'secondary)
                      (eq new-type 'primary))
                 (or (not type) (eq type 'plain)))
            (delete-region start end)
	    (insert (pavel/quote
                     new-type
                     (if type
                         (substring text 1 -1)
                       text)))))

      (insert (pavel/quote new-type))
      (backward-char 1))))

(defun pavel/insert-dash-character (length)
  "inserts em dash, en dash or hyphen character according to `length'"

  (insert-char
   (cond ((eq 3 length)
          (char-from-name "EM DASH"))
         ((eq 2 length)
          (char-from-name "EN DASH"))
         (t
          (char-from-name "HYPHEN")))
   1 t))

;;;###autoload
(defun pavel/smart-dash (arg)
  "inserts em dash, en dash or hyphen character according to numeric
prefix argument or replaces current region with dash in case of it is
sequence of hyphens"
  (interactive "p")

  (if (use-region-p)
      (let* ((start (region-beginning))
             (end (region-end))
             (text (buffer-substring start end)))
        (when (string-match "^[-]+$" text)
          (delete-region start end)
	  (pavel/insert-dash-character (length text))))

    (pavel/insert-dash-character (if arg arg 3))))

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
