;;; search-aux.el --- search, replace and grep symbols -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pavel Sokolov
;; All rights reserved.

;; Author: Pavel Sokolov <pavel.zepp@gmail.com>
;; Maintainer: Pavel Sokolov <pavel.zepp@gmail.com>
;; Created: 2025

;; This file is NOT part of GNU Emacs.

(require 'thingatpt)
(require 'vc-git)
(require 'project)

;;;###autoload
(defcustom search/dir-tree-engines
  '((fundamental-mode . (rgrep . search/compose-rgrep-args)))
  "alist to be used by `search/thing-dir-tree' to find a
engine for a current major mode and perform a search in a directory tree"
  :type 'sexp)

;;;###autoload
(defcustom search/dir-tree-root-providers '(search/project-root vc-git-root)
  "list of functions to provide a root of a directory tree for a
buffer. Function is executed in the context of the buffer and must
return nonempty string or nil"
  :type '(repeat function))

;;;###autoload
(defcustom search/prog-modes '(prog-mode sgml-mode nxml-mode)
  "modes those considered as programming modes"
  :type '(repeat function))

;;;###autoload
(defcustom search/word-trim-alist
  '(("[a-zA-Z’'\\-]+" . "[’'s]+")
    ("[а-яА-Я\\-]" . "\\(с[ья]\\)\\|\\([тш][ье]\\)\\|\\([аяеыо]?ми?\\)\
\\|\\(ов\\)\\|\\([аяы]х\\)\\|\\([ауоыийэяюёеь]+\\)"))
  "regular expressions to trim a word ending for better text search"
  :type '(repeat (cons regexp regexp)))

(defvar-local search/scope nil
  "buffer local variable that keeps scope for `search/thing-dir-tree'")

;;;###autoload
(defun search/trim-word (word)
  "trims WORD ending for better text search. It returns original
WORD in case of `string-trim-right' returns empty string."

  (let ((regexp (seq-some
               #'(lambda (cell)
                   (when (string-match (car cell) word)
                     (cdr cell)))
               search/word-trim-alist)))
    (if regexp
        (let ((base (string-trim-right word regexp)))
          (if (string-empty-p base)
              word
            base))
      word)))

(defun search/compose-rgrep-args (thing scope)
  "Composes an argument list for `rgrep' and `rzgrep' commands"

  (let ((regexp
         (format
          (cond
           ((search/is thing 'symbol) "\\b%s\\b")
           ((search/is thing 'word) "\\b%s\\w*\\b")
           ((search/is thing 'filename) "%s\\b")
           (t "%s"))
          (if (search/is thing 'word)
              (search/trim-word (cdr thing))
            (search/quote thing))))
        (ext (alist-get 'ext scope "*")))
    (list regexp
          (concat "*." ext)
          (alist-get 'root scope))))

(defun search/project-root (path)
  "Looks for a root of a directory tree using `project-root'"

  (let ((proj (project-current)))
    (when proj (project-root proj))))

(defun search/dir-tree-engine (&optional mode)
  "Searches engine for MODE. If one is not specified then
`major-mode' is used"

  (let ((effective-mode (or mode major-mode))
        (fallback (alist-get
                   'fundamental-mode
                   search/dir-tree-engines)))
    (or (seq-some #'(lambda (entry)
                      (when (provided-mode-derived-p
                             effective-mode
                             (car entry))
                        (cdr entry)))
                  search/dir-tree-engines)
        fallback)))

(defun search/get-thing (thing &optional overlay-secs trim)
  "returns thing at point as `cons'"

  (let ((bounds (bounds-of-thing-at-point thing)))
    (when overlay-secs
      (let ((overlay (make-overlay (car bounds) (cdr bounds))))
        (overlay-put overlay 'face 'region)
        (overlay-put overlay 'evaporate t)
        (run-at-time overlay-secs
                     nil
                     #'(lambda ()
                         (delete-overlay overlay)))))
    (cons thing
          (let ((string (buffer-substring-no-properties
                         (car bounds)
                         (cdr bounds))))
            (if trim
                (string-trim string trim trim)
              string)))))

(defun search/thing-at-point (&optional no-input)
  "Intends to pick a symbol at point or something else if there is
an active region. Initiates minibuffer input if NO-INPUT is nil
as a last resort."

  (cond
   ;; check region at first in case of only part of a thing at point should be
   ;; searched.
   ((use-region-p)
    (let ((substring (buffer-substring-no-properties
                      (region-beginning) (region-end)))
          (deactivate-mark 'dont-save))
      (deactivate-mark)
      (cons 'string substring)))

   ((thing-at-point 'email)
    (search/get-thing 'email 1 "[<>]"))

   ((thing-at-point 'uuid)
    (search/get-thing 'uuid 1))

   ((and (derived-mode-p 'dired-mode)
         (thing-at-point 'filename))
    (search/get-thing 'filename 1))

   ;; `apply' is for compatibility reasons
   ((and (apply #'derived-mode-p search/prog-modes)
         (thing-at-point 'symbol))
    (search/get-thing 'symbol 1))

   ((and (derived-mode-p 'text-mode)
         (thing-at-point 'word))
    (search/get-thing 'word 1))

   ((not no-input)
    (let* ((line (string-trim (thing-at-point 'line t)))
           (string (read-string "Specify thing from line: " line nil nil t)))
      (if (string-empty-p string)
          (user-error "Thing lookup is failed and input is not provided")
        (cons 'string string))))))

(defmacro search/is (thing type)
  `(equal (car ,thing) ,type))

(defmacro search/quote (thing)
  `(regexp-quote (cdr ,thing)))

;;;###autoload
(defun search/thing-replace (thing new-name)
  "it renames THING to NEW-NAME in a current buffer using
`query-replace-regexp'"

  (interactive
   (let ((thing (search/thing-at-point)))
     (list thing
           (read-string (format "Rename '%s' to: " (cdr thing))
                        (cdr thing) nil nil t))))

  (if (buffer-modified-p)
      (user-error "Rename of '%s' is aborted since buffer '%s' is modified"
                  (cdr thing) (buffer-name (current-buffer)))
    (save-excursion
      (let ((thing-regexp (cond ((search/is thing 'symbol)
                                 (format "\\_<%s\\_>" (cdr thing)))
                                ((search/is thing 'word)
                                 (format "\\b%s" (cdr thing)))
                                (t (format "\\b%s\\b" (search/quote thing))))))
        (goto-char (point-min))
        (query-replace-regexp thing-regexp new-name)))))

(defun search/root-dir (path &optional fallback)
  "iterates over `search/dir-tree-root-providers' to find root
directory."

  (let ((result (or (seq-some
                     #'(lambda (fun)
                         (funcall fun path))
                     search/dir-tree-root-providers)
                    fallback)))
    (when result
      (expand-file-name result))))

(defun search/get-scope (buffer)
  "finds BUFFER scope out and caches one to `search/scope'"

  (with-current-buffer buffer
    (if search/scope
        search/scope
      (let* ((file-path (buffer-file-name))
             (dir-path (expand-file-name default-directory))
             (root (search/root-dir
                    (or file-path
                        dir-path)
                    dir-path))
             (local (if (string= root dir-path)
                        nil
                      (substring dir-path (length root)))))
        (setq search/scope
              (let ((scope `((root . ,root))))
                (when local
                  (push `(local . ,local) scope))
                (when file-path
                  (push `(ext . ,(file-name-extension file-path))
                        scope))
                scope))))))

(defun search/modify (scope strategy)
  "modifies SCOPE according to STRATEGY"

  (if (not (eq strategy 'same-files))
      (assq-delete-all 'ext scope)
    scope))

;;;###autoload
(defun search/thing-dir-tree (thing scope)
  "searches THING in the SCOPE using a engine from
`search/dir-tree-engines' alist. If prefix argument is not nil then
search only files with a same extension."

  (interactive
   (list (search/thing-at-point)
         (search/modify
          (copy-alist (search/get-scope (current-buffer)))
          (when current-prefix-arg 'same-files))))

  (let* ((engine (search/dir-tree-engine))
         (args (funcall (cdr engine)
                        thing
                        scope)))
    ;; special variables to be overridden since they affect execution context
    (let ((default-directory (alist-get 'root scope))
          (current-prefix-arg nil))
      (apply (car engine) args))))

(provide 'pavel-search-aux)
