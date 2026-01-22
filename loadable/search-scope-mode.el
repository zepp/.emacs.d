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

(defcustom search-scope-grep-engines
  '((fundamental-mode . (rgrep . search-scope-compose-rgrep-args)))
  "alist to be used by `search-scope-grep' to find a engine for a
current major mode and perform a search in a directory tree"
  :group 'search-scope
  :type 'sexp)

(defcustom search-scope-root-functions '(search-scope-project-root vc-git-root)
  "set of functions to discover a root of a directory tree for a
buffer. Function is executed in the context of the buffer and must
return nonempty string or nil"
  :group 'search-scope
  :type '(repeat function))

(defcustom search-scope-symbolic-modes '(prog-mode sgml-mode nxml-mode conf-mode)
  "modes to search symbols rather then words"
  :group 'search-scope
  :type '(repeat function))

(defcustom search-scope-thing-flash-seconds 1
  "number of seconds to flash a thing at point using overlay"
  :group 'search-scope
  :type 'natnum)

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

(defcustom search-scope-word-min-length 3
  "It specifies a minimal length of a word to be trimmed"
  :group 'search-scope
  :type 'natnum)

(defcustom search-scope-indexers '(search-scope-index-project-files)
  "set of functions to build a file list of a
scope. `search-scope-index-files' sequentially calls entries
until non-empty list is returned. Indexer receives a root and a
regular expression to filter result and returns the list of
relative paths or nil."
  :group 'search-scope
  :type '(repeat function))

(defcustom search-scope-markers '("Makefile" "package.json" "project.json")
  "list of file names that marks a scope"
  :group 'search-scope
  :type '(repeat string))

(defvar search-scope-searched-things '()
  "alist to keep a history of searched things")

(defconst search-scope-history '()
  "special variable to be overridden in a lexical scope to hold a
simple history for minibuffer functions. See also
`search-scope-simple-history'.")

(defvar-local search-scope nil
  "buffer local variable that keeps scope for `search-scope-grep-thing'")

;;;###autoload
(defun search-scope-register-symbolic-engine (function args-function)
  "registers an engine for all modes from `search-scope-symbolic-modes'
alist."

  (search-scope-register-engine
   function
   args-function
   search-scope-symbolic-modes))

;;;###autoload
(defun search-scope-register-engine (function args-function &optional object)
  "it registers a new grep engine in `search-scope-grep-engines' alist. If
OBJECT is null then default search engine is overridden otherwise a
major mode or a list of modes is expected"

  (cond
   ((or (null object) (functionp object))
    (setf (alist-get (or object 'fundamental-mode) search-scope-grep-engines)
          (cons function args-function)))
   ((listp object)
    (dolist (mode object)
      (setf (alist-get mode search-scope-grep-engines)
            (cons function args-function))))
   (t
    (user-error "can't register engine: unsupported object type %s"
                (type-of object)))))

(defmacro search-scope-is (thing type)
  `(equal (cdr ,thing) ,type))

(defmacro search-scope-quote (thing)
  `(regexp-quote (car ,thing)))

(defun search-scope-compose-rgrep-args (thing scope)
  "Composes an argument list for `rgrep' and `rzgrep' commands"

  (let ((regexp (search-scope-thing-to-regexp thing t))
        (ext (alist-get 'ext scope "*")))
    (list (if regexp regexp (search-scope-quote thing))
          (concat "*." ext)
          (search-scope-absolute-path scope))))

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
           (search-scope-trim-word (car thing))
         (search-scope-quote thing))))))

(defun search-scope-to-path-regexp (scope)
  "forms a relative path regexp to perform a search in SCOPE"

  (let ((path (alist-get 'relative scope))
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
  "Looks for a root directory of PATH using `project-root'"

  (let ((proj (project-current nil (file-name-directory path))))
    (when proj (project-root proj))))

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

(defun search-scope-index-project-files (root &optional regexp)
  "Returns file list of project in ROOT."

  (let ((proj (project-current nil root)))
    (when proj
      (let ((paths (mapcar
                    #'(lambda (dir)
                        (file-relative-name dir root))
                    (project-files proj))))
        (if regexp
            (seq-filter
             #'(lambda (path)
                 (string-match regexp path))
             paths)
          paths)))))

(defun search-scope-index-files (scope &optional regexp)
  "Returns a directory list inside of the SCOPE using functions
from the `search-scope-indexers'. List contains relative paths
filtered using REGEXP."
  (let ((root (alist-get 'root scope)))
    (seq-some #'(lambda (fun)
                  (funcall fun root regexp))
              search-scope-indexers)))

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

(defun search-scope-construct (root &optional path)
  "Constucts a new scope from ROOT and PATH."

  (let ((scope `((root . ,root)
                 (strategy . default)))
        (dir (when path (file-name-directory path))))
    (cond
     ((and dir (file-name-absolute-p dir))
      (let ((relative (file-relative-name dir root)))
        (push `(absolute . ,dir) scope)
        (push `(relative . ,relative) scope)))
     (dir
      (let ((absolute (expand-file-name dir root)))
        (push `(absolute . ,absolute) scope)
        (push `(relative . ,dir) scope)))
     (t
      (push `(absolute . ,root) scope)))
    scope))

(defun search-scope-get (buffer)
  "finds BUFFER scope out and caches one to `search-scope'"

  (with-current-buffer buffer
    (if search-scope
        search-scope
      (let* ((path (buffer-file-name))
             (dir (or (file-name-directory path)
                      (expand-file-name default-directory)))
             (root (search-scope-discover-root
                    (or path dir)
                    dir)))
        (setq search-scope (search-scope-construct root))))))

(defun search-scope-adjust (scope &optional path dwim)
  "adjusts a copy of SCOPE according to user input."

  (let ((scope (copy-alist scope)))
    (if dwim
        scope
      (let ((relative (alist-get 'relative scope))
            (dir (or (search-scope-completing-read-dir scope nil t)
                     (search-scope-read-dir scope nil t))))
        (if dir
            (setf (alist-get 'relative scope) dir)
          (setf scope (assq-delete-all 'relative scope)))

        (let ((strategy (search-scope-read-strategy scope)))
          (setf (alist-get 'strategy scope)
                strategy)
          (when (eq strategy 'same-files)
            (if path
                (push `(ext . ,(file-name-extension path))
                      scope)
              (push `(ext . ,(read-string "File extension: "))
                    scope))))))
    scope))

(defun search-scope-absolute-path (scope &optional abbreviate)
  "Returns an absolute directory path of SCOPE"

  (let ((root (alist-get 'root scope))
        (relative (alist-get 'relative scope ".")))
    (if abbreviate
        (abbreviate-file-name (expand-file-name relative root))
      (expand-file-name relative root))))

(defun search-scope-closest-dir (path dirs)
  "finds PATH closest directory from DIRS list."

  (seq-reduce #'(lambda (accum dir)
                  (if (and (string-match-p (regexp-quote dir) path)
                           (or (null accum)
                               (string> dir accum)))
                      dir
                    accum))
              dirs nil))

(defun search-scope-marked-dirs (scope)
  "builds a direcotry list of SCOPE using
`search-scope-markers'."

  (let* ((quoted-markers (mapcar #'regexp-quote
                                 search-scope-markers))
         (regexp (string-join quoted-markers "\\|"))
         (dirs (mapcar #'file-name-directory
                       (search-scope-index-files scope regexp))))
    (remove nil (delete-dups dirs))))

(defun search-scope-completing-read-dir (scope &optional prompt require-match)
  "Reads a relative directory path with completion. List is build by
`search-scope-marked-dirs'"

  (let ((root (alist-get 'root scope))
        (dirs (search-scope-marked-dirs scope)))
    (when (length> dirs 0)
      (let* ((dir (search-scope-closest-dir
                   (file-relative-name default-directory root)
                   dirs))
             (default (alist-get 'relative scope dir)))
        (completing-read
         (or prompt
             (format "%s directory (%s) : "
                     (abbreviate-file-name root) default))
         dirs nil require-match nil nil default)))))

(defun search-scope-read-dir (scope &optional prompt require-match)
  "Reads a relative directory path using `read-directory-name'."

  (let* ((root (alist-get 'root scope))
         (relative (alist-get 'relative scope
                           (file-relative-name
                            default-directory root)))
         (default (expand-file-name relative root))
         (dir (expand-file-name
               (read-directory-name
                (or prompt "Specify directory: ")
                default default require-match))))
    (if (and (string-match-p (regexp-quote root) dir)
             (not (string= dir root)))
        (file-relative-name dir root)
      nil)))

(defun search-scope-read-strategy (scope)
  "Reads a search strategy for SCOPE."

  (let* ((relative (alist-get 'relative scope))
         (strategies '(default same-files))
         (strategy (alist-get 'strategy scope (if relative 'default 'same-files))))
    (intern
     (completing-read
      (format "%s search strategy (%s): "
              (search-scope-absolute-path scope t)
              strategy)
      strategies nil t nil nil (symbol-name strategy)))))

(defun search-scope-read-thing (&optional prompt initial)
  "Reads a thing from minibuffer"

  (let* ((search-scope-history (search-scope-simple-history))
         (string (completing-read
                  (or prompt
                      "Specify a thing: ")
                  search-scope-history
                  nil nil initial
                  'search-scope-history
                  (car search-scope-history))))
    (let* ((alist search-scope-searched-things)
           (thing (when string (assoc string alist))))
      (if thing
          (setq search-scope-searched-things
                (delete thing alist))
        (setq thing (cons string 'string)))
      (push thing search-scope-searched-things)
      thing)))

(defun search-scope-add-to-history (thing)
  "records THING to `search-scope-searched-things'"

  (let ((alist search-scope-searched-things)
        (length (length search-scope-searched-things))
        (max-alist-size 30))
    (when (>= length max-alist-size)
      (setf alist (nbutlast alist (- (1+ length) max-alist-size))))
    (setf search-scope-searched-things
          (assoc-delete-all (car thing) alist)))
  (push thing search-scope-searched-things)
  thing)

(defun search-scope-simple-history (&optional omit-first-thing)
  "It transforms `search-scope-searched-things' to a simple list of
thing names"

  (let ((names (mapcar #'car search-scope-searched-things)))
    (if omit-first-thing
        (cdr names)
      names)))

(defun search-scope-get-thing (thing-type &optional trim)
  "returns thing at point as `cons'"

  (let ((bounds (bounds-of-thing-at-point thing-type)))
    (when search-scope-thing-flash-seconds
      (let ((overlay (make-overlay (car bounds) (cdr bounds))))
        (overlay-put overlay 'face 'region)
        (overlay-put overlay 'evaporate t)
        (run-at-time search-scope-thing-flash-seconds
                     nil
                     #'(lambda ()
                         (delete-overlay overlay)))))
    (cons
     (let ((string (buffer-substring-no-properties
                    (car bounds)
                    (cdr bounds))))
       (if trim
           (string-trim string trim trim)
         string))
     thing-type)))

(defun search-scope-thing-at-point (&optional no-input)
  "Intends to pick a symbol at point or something else if there is
an active region. Initiates minibuffer input if NO-INPUT is nil
as a last resort."

  (let ((thing
         (cond
          ;; check region at first in case of only part of a thing at point should be
          ;; searched.
          ((use-region-p)
           (let ((substring (buffer-substring-no-properties
                             (region-beginning) (region-end)))
                 (deactivate-mark 'dont-save))
             (deactivate-mark)
             (cons substring 'string)))

          ((thing-at-point 'email)
           (search-scope-get-thing 'email "[<>]"))

          ((thing-at-point 'uuid)
           (search-scope-get-thing 'uuid))

          ((and (derived-mode-p 'dired-mode)
                (thing-at-point 'filename))
           (search-scope-get-thing 'filename))

          ;; `apply' is for compatibility reasons
          ((and (apply #'derived-mode-p search-scope-symbolic-modes)
                (thing-at-point 'symbol))
           (search-scope-get-thing 'symbol))

          ((and (derived-mode-p 'text-mode)
                (thing-at-point 'word))
           (search-scope-get-thing 'word))

          ((not no-input)
           (search-scope-read-thing
            "Specify a thing from line: "
            (string-trim (thing-at-point 'line t)))))))
    (search-scope-add-to-history thing)))

;;;###autoload
(defun search-scope-replace (thing new-name)
  "it renames THING to NEW-NAME in a current buffer using
`query-replace-regexp'"

  (interactive
   (let ((thing (search-scope-thing-at-point))
         (search-scope-history (search-scope-simple-history t)))
     (list thing
           (read-string (format "Rename '%s' to: " (car thing))
                        (car thing) 'search-scope-history nil t))))

  (if (buffer-modified-p)
      (user-error "Rename of '%s' is aborted since buffer '%s' is modified"
                  (car thing) (buffer-name (current-buffer)))
    (save-excursion
      (let ((thing-regexp (cond ((search-scope-is thing 'symbol)
                                 (format "\\_<%s\\_>" (car thing)))
                                ((search-scope-is thing 'word)
                                 (format "\\b%s" (car thing)))
                                (t (format "\\b%s\\b" (search-scope-quote thing))))))
        (goto-char (point-min))
        (query-replace-regexp thing-regexp new-name)))))

;;;###autoload
(defun search-scope-grep (thing scope)
  "searches THING in SCOPE using an engine from
`search-scope-grep-engines' alist. If prefix argument is not nil then
user is queried to adjust scope and searching strategy. `search-scope'
is updated with a new value."

  (interactive
   (list
    ;; double universal prefix argument
    (if (= (prefix-numeric-value current-prefix-arg) 16)
        (search-scope-read-thing)
      (search-scope-thing-at-point))
    (search-scope-adjust
     (search-scope-get (current-buffer))
     (buffer-file-name)
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
