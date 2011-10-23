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
  :type 'list)

(defvar-local search/scope nil
  "buffer local variable that keeps scope for `search/thing-dir-tree'")

(defun search/compose-rgrep-args (thing scope)
  "Composes an argument list for `rgrep' and `rzgrep' commands"

  (let ((regexp
         (format
          (if (search/is thing 'symbol) "\\b%s\\b" "%s")
          (search/quote thing)))
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

(defun search/thing-at-point (&optional no-input)
  "Intends to pick a symbol at point or something else if there is
an active region. Initiates minibuffer input if NO-INPUT is nil
as a last resort."

  (cond
   ;; check region at first in case of only part of a thing at point should be
   ;; searched.
   ((use-region-p)
    (let* ((substring (buffer-substring-no-properties
                       (region-beginning) (region-end)))
           (thing (cons 'string substring))
           (deactivate-mark 'dont-save))
      (deactivate-mark)
      thing))

   ((thing-at-point 'symbol)
    (cons 'symbol (thing-at-point 'symbol t)))

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
      (let ((thing-regexp (if (search/is thing 'symbol)
                              (format "\\_<%s\\_>" (cdr thing))
                            (format "\\b%s\\b" (search/quote thing)))))
        (goto-char (point-min))
        (query-replace-regexp thing-regexp new-name)))))

(defun search/root-dir (path &optional fallback)
  "iterates over `search/dir-tree-root-providers' to find root
directory."

  (or (seq-some #'(lambda (fun)
                    (funcall fun path))
                search/dir-tree-root-providers)
      fallback))

(defun search/get-scope (buffer)
  "finds BUFFER scope out and caches one to `search/scope'"

  (with-current-buffer buffer
    (if search/scope
        search/scope
      (let* ((file-name (buffer-file-name))
             (root (search/root-dir
                    (or file-name
                        default-directory)
                    default-directory))
             (len (length (expand-file-name root)))
             (local (if (string= root default-directory)
                        nil
                      (substring default-directory len))))
        (setq search/scope
              (let ((scope `((root . ,root)(type . root))))
                (when local
                  (push `(local . ,local) scope))
                (when file-name
                  (push `(ext . ,(file-name-extension file-name))
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
