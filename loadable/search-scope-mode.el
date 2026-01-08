;;; search-scope-mode.el --- grep and replace things in a scope -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pavel Sokolov
;; All rights reserved.

;; Author: Pavel Sokolov <pavel.zepp@gmail.com>
;; Maintainer: Pavel Sokolov <pavel.zepp@gmail.com>
;; Created: 2025

;; This file is NOT part of GNU Emacs.

(require 'thingatpt)
(require 'vc-git)
(require 'project)

(defgroup search-scope nil "Main group")

;;;###autoload
(defcustom search-scope-grep-engines
  '((fundamental-mode . (rgrep . search-scope-compose-rgrep-args)))
  "alist to be used by `search-scope-grep' to find a engine for a
current major mode and perform a search in a directory tree"
  :group 'search-scope
  :type 'sexp)

;;;###autoload
(defcustom search-scope-root-functions '(search-scope-project-root vc-git-root)
  "set of functions to discover a root of a directory tree for a
buffer. Function is executed in the context of the buffer and must
return nonempty string or nil"
  :group 'search-scope
  :type '(repeat function))

;;;###autoload
(defcustom search-scope-symbol-modes '(prog-mode sgml-mode nxml-mode conf-mode)
  "modes to search symbols rather then words"
  :group 'search-scope
  :type '(repeat function))

;;;###autoload
(defcustom search-scope-thing-flash-seconds 1
  "number of seconds to flash a thing at point using overlay"
  :group 'search-scope
  :type 'natnum)

;;;###autoload
(defcustom search-scope-word-trim-alist
  `(("[a-zA-Z’'\\-]+" . ,(string-join '("[’']?s" "y" "ship" "ment")
                                      "\\|"))
    ("[а-яА-Я\\-]" . ,(string-join '("с[ья]" "[тш][ье]" "[аяеыо]?ми?"
                                     "ов" "[аяы]х"
                                     "[ауоыийэяюёеь]+")
                                   "\\|")))
  "regular expressions to trim a word ending for better text search"
  :group 'search-scope
  :type '(repeat (cons regexp regexp)))

;;;###autoload
(defcustom search-scope-word-min-length 3
  "It specifies a minimal length of a word to be trimmed"
  :group 'search-scope
  :type 'natnum)

;;;###autoload
(defcustom search-scope-dir-indexers '(search-scope-project-dirs)
  "set of functions to build a directory list of a
scope. `search-scope-index-dirs' sequentially calls entries until
non-empty list is returned. Indexer receives a root and a regular
expression to filter result and returns the list of relative
paths or nil."
  :group 'search-scope
  :type '(repeat function))

(defvar search-scope-thing-history '()
  "history list to keep searched things")

(defvar-local search-scope nil
  "buffer local variable that keeps scope for `search-scope-grep-thing'")

(defun search-scope-compose-rgrep-args (thing scope)
  "Composes an argument list for `rgrep' and `rzgrep' commands"

  (let ((regexp (search-scope-thing-to-regexp thing t))
        (ext (alist-get 'ext scope "*")))
    (list (if regexp regexp (regexp-quote (cdr thing)))
          (concat "*." ext)
          (search-scope-absolute-dir-path scope))))

;;;###autoload
(defun search-scope-thing-to-regexp (thing &optional trim-word)
  "forms a regexp to perform a search of THING"

  (let ((template (cond
                   ((search-scope-is thing 'symbol) "\\b%s\\b")
                   ((search-scope-is thing 'word)
                    (if trim-word "\\b%s\\w*\\b" "\\b%s\\b"))
                   ((search-scope-is thing 'filename) "%s\\b"))))
    (when template
      (format
       template
       (if (and (search-scope-is thing 'word) trim-word)
           (search-scope-trim-word (cdr thing))
         (search-scope-quote thing))))))

;;;###autoload
(defun search-scope-to-path-regexp (scope)
  "forms a relative path regexp to perform a search in SCOPE"

  (let ((path (alist-get 'local scope))
        (ext (alist-get 'ext scope))
        (strategy (alist-get 'strategy scope)))
    (cond
     ((and (eq strategy 'same-files) path ext)
      (format "%s.*\\.%s$" (regexp-quote path) (regexp-quote ext)))
     ((and (eq strategy 'same-files) ext)
      (format "\\.%s$" (regexp-quote ext)))
     (path
      (format "%s" (regexp-quote path))))))

(defun search-scope-trim-word (word)
  "trims WORD ending for better text search. It returns original
WORD in case of original WORD length or trimmed WORD length is
less then `search-scope-word-min-length'"

  (if (length< word search-scope-word-min-length)
      word
    (let ((regexp (seq-some
                   #'(lambda (cell)
                       (when (string-match (car cell) word)
                         (cdr cell)))
                   search-scope-word-trim-alist)))
      (if regexp
          (let ((trimmed (string-trim-right word regexp)))
            (if (length< trimmed search-scope-word-min-length)
                word
              trimmed))
        word))))

(defun search-scope-project-root (path)
  "Looks for a root of a directory tree using `project-root'"

  (let ((proj (project-current nil (file-name-directory path))))
    (when proj (project-root proj))))

(defun search-scope-project-dirs (root &optional regexp)
  "Returns a directory list inside a project in ROOT."

  (let ((proj (project-current nil root)))
    (when proj
      (let* ((dirs (delete-dups
                    (mapcar #'file-name-directory
                            (project-files proj))))
             (relatives (mapcar
                         #'(lambda (dir)
                             (file-relative-name dir root))
                         dirs)))
        (sort
         (remove
          "./"
          (if regexp
              (seq-filter
               #'(lambda (dir)
                   (string-match regexp dir))
               relatives)
            relatives))
         #'string>)))))

(defun search-scope-index-dirs (scope &optional regexp)
  "Returns a directory list inside of the SCOPE using functions
from the `search-scope-dir-indexers'. List contains relative
paths filtered using REGEXP."
  (let ((root (alist-get 'root scope)))
    (seq-some #'(lambda (fun)
                  (funcall fun root regexp))
              search-scope-dir-indexers)))

(defun search-scope-read-relative-dir (root &optional dirs initial)
  "Reads a relative directory path. If DIRS is not empty then read
is completing."

  (let* ((dir (expand-file-name (or initial "") root))
         (relative-dir
          (if (length> dirs 0)
              (completing-read
               (format "Specify directory (%i): " (length dirs))
               dirs nil t initial)
            (file-relative-name
             (read-directory-name
              "Specify directory: "
              dir dir t)
             root))))
    (setf dir (expand-file-name relative-dir root))
    (if (and (not (string= root dir))
             (string-match-p (regexp-quote root) dir))
        relative-dir
      nil)))

(defun search-scope-get-engine (&optional mode)
  "Searches engine for MODE. If one is not specified then
`major-mode' is used"

  (let ((effective-mode (or mode major-mode))
        (fallback (alist-get
                   'fundamental-mode
                   search-scope-grep-engines)))
    (or (seq-some #'(lambda (entry)
                      (when (provided-mode-derived-p
                             effective-mode
                             (car entry))
                        (cdr entry)))
                  search-scope-grep-engines)
        fallback)))

(defun search-scope-discover-root (path &optional fallback)
  "iterates over `search-scope-root-functions' to find a root
directory."

  (let ((result (or (seq-some
                     #'(lambda (fun)
                         (funcall fun path))
                     search-scope-root-functions)
                    fallback)))
    (when result
      (expand-file-name result))))

(defun search-scope-absolute-dir-path (scope)
  "Returns an absolute directory path of SCOPE"

  (let ((root (alist-get 'root scope))
        (local (alist-get 'local scope ".")))
    (expand-file-name local root)))

(defun search-scope-add-to-history (thing-value)
  "records THING to `search-scope-thing-history'"

  (let ((history-delete-duplicates t))
    (add-to-history 'search-scope-thing-history thing-value 32)))

(defun search-scope-get-thing (thing &optional trim)
  "returns thing at point as `cons'"

  (let ((bounds (bounds-of-thing-at-point thing)))
    (when search-scope-thing-flash-seconds
      (let ((overlay (make-overlay (car bounds) (cdr bounds))))
        (overlay-put overlay 'face 'region)
        (overlay-put overlay 'evaporate t)
        (run-at-time search-scope-thing-flash-seconds
                     nil
                     #'(lambda ()
                         (delete-overlay overlay)))))
    (cons thing
          (let* ((string (buffer-substring-no-properties
                          (car bounds)
                          (cdr bounds)))
                 (trimmed (if trim
                              (string-trim string trim trim)
                            string)))
            (search-scope-add-to-history trimmed)
            trimmed))))

(defun search-scope-thing-at-point (&optional no-input)
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
      (search-scope-add-to-history substring)
      (cons 'string substring)))

   ((thing-at-point 'email)
    (search-scope-get-thing 'email "[<>]"))

   ((thing-at-point 'uuid)
    (search-scope-get-thing 'uuid))

   ((and (derived-mode-p 'dired-mode)
         (thing-at-point 'filename))
    (search-scope-get-thing 'filename))

   ;; `apply' is for compatibility reasons
   ((and (apply #'derived-mode-p search-scope-symbol-modes)
         (thing-at-point 'symbol))
    (search-scope-get-thing 'symbol))

   ((and (derived-mode-p 'text-mode)
         (thing-at-point 'word))
    (search-scope-get-thing 'word))

   ((not no-input)
    (search-scope-read-thing
     "Specify a thing from line: "
     (string-trim (thing-at-point 'line t))))))

(defun search-scope-read-thing (prompt &optional initial)
  "reads a string thing from minibuffer"

  (let ((string (completing-read
                 prompt
                 search-scope-thing-history
                 nil nil initial 'search-scope-thing-history)))
    (if (string-empty-p string)
        (user-error "no input is provided")
      (cons 'string (string-trim string)))))

(defmacro search-scope-is (thing type)
  `(equal (car ,thing) ,type))

(defmacro search-scope-quote (thing)
  `(regexp-quote (cdr ,thing)))

;;;###autoload
(defun search-scope-replace (thing new-name)
  "it renames THING to NEW-NAME in a current buffer using
`query-replace-regexp'"

  (interactive
   (let ((thing (search-scope-thing-at-point)))
     (list thing
           (read-string (format "Rename '%s' to: " (cdr thing))
                        (cdr thing) 'search-scope-thing-history nil t))))

  (if (buffer-modified-p)
      (user-error "Rename of '%s' is aborted since buffer '%s' is modified"
                  (cdr thing) (buffer-name (current-buffer)))
    (save-excursion
      (let ((thing-regexp (cond ((search-scope-is thing 'symbol)
                                 (format "\\_<%s\\_>" (cdr thing)))
                                ((search-scope-is thing 'word)
                                 (format "\\b%s" (cdr thing)))
                                (t (format "\\b%s\\b" (search-scope-quote thing))))))
        (goto-char (point-min))
        (query-replace-regexp thing-regexp new-name)))))

(defun search-scope-get (buffer)
  "finds BUFFER scope out and caches one to `search-scope'"

  (with-current-buffer buffer
    (if search-scope
        search-scope
      (let* ((file-path (buffer-file-name))
             (dir-path (expand-file-name default-directory))
             (root (search-scope-discover-root
                    (or file-path
                        dir-path)
                    dir-path)))
        (setq search-scope
              (let ((scope `((root . ,root)
                             (strategy . default))))
                (when file-path
                  (push `(ext . ,(file-name-extension file-path))
                        scope))
                scope))))))

(defun search-scope-adjust (scope &optional dwim)
  "adjusts a copy of SCOPE according to user input."

  (let ((scope (copy-alist scope)))
    (if dwim
        scope
      (let* ((root (alist-get 'root scope))
             (local (alist-get 'local scope))
             (path (search-scope-read-relative-dir
                    root
                    (search-scope-index-dirs scope)
                    (cond (local local)
                          ((not (string= default-directory root))
                           (file-relative-name
                            default-directory
                            root))))))
        (if path
            (setf (alist-get 'local scope) path)
          (setf scope (assq-delete-all 'local scope)))

        (let* ((strategies '(default same-files))
               (strategy (alist-get 'strategy scope 'default))
               (ext (alist-get 'ext scope))
               ;; `substring' is to copy a name of symbol
               (initial (substring (symbol-name strategy))))
          (setf strategy (intern
                          (completing-read
                           "Search strategy: "
                           strategies nil t
                           initial)))
          (setf (alist-get 'strategy scope)
                strategy)
          (when (and (eq strategy 'same-files)
                     (not ext))
            (setf (alist-get 'ext scope)
                  (read-string "File extension: "))))))
    scope))

;;;###autoload
(defun search-scope-grep (thing scope)
  "searches THING in SCOPE using an engine from
`search-scope-grep-engines' alist. If prefix argument is not nil
then user is queried to adjust local scope and searching
strategy. `search-scope' is updated with a new value."

  (interactive
   (list
    ;; double universal prefix argument
    (if (= (prefix-numeric-value current-prefix-arg) 16)
        (search-scope-read-thing "Specify a string: ")
      (search-scope-thing-at-point))
    (search-scope-adjust
     (search-scope-get (current-buffer))
     (not current-prefix-arg))))

  (let* ((engine (search-scope-get-engine))
         (args (funcall (cdr engine)
                        thing
                        scope)))
    (setf search-scope scope)
    ;; special variables to be overridden since they affect execution context
    (let ((default-directory (alist-get 'root scope))
          (current-prefix-arg nil))
      (apply (car engine) args))))

(provide 'search-scope-mode)
