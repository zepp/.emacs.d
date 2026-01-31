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

;;;###autoload
(define-minor-mode search-scope-mode
  "Minor mode to search things in a scope")

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

(defcustom search-scope-marker-regexps '("Makefile" "CMakeLists\\.txt$"
                                         "package\\.json$" "project\\.json$")
  "list of file names that marks a scope"
  :group 'search-scope
  :type '(repeat string))

(defcustom search-scope-root-strategy 'same-files
  "list to keep discovered scopes"
  :group 'search-scope
  :type 'symbol)

(defcustom search-scope-max-history-size 30
  "maximal size of `search-scope-searched-things'."
  :group 'search-scope
  :type 'natnum)

(defcustom search-scope-display-pair-function
  #'display-buffer-pop-up-window
  "Function to form an action passed to `display-buffer' in
`search-scope-find-pair'."
  :group 'search-scope
  :type 'function)

(defvar search-scope-searched-things '()
  "alist to keep a history of searched things")

(defconst search-scope-history '()
  "special variable to be overridden in a lexical scope to hold a
simple history for minibuffer functions. See also
`search-scope-simple-history'.")

(defvar search-scope-list '()
  "list to keep discovered scopes")

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
   ((consp object)
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

(defun search-scope-index-project-files (scope exclude-dirs &optional regexp)
  "Filters a list produced by `project-files'"

  (when regexp
    (cl-assert (memq (type-of regexp) '(cons string))))

  (let* ((root (alist-get 'root scope))
         (absolute (alist-get 'absolute scope))
         (proj (project-current nil root))
         (exclude-regexp (when exclude-dirs
                           (string-join (mapcar #'regexp-quote exclude-dirs)
                                        "\\|"))))
    (when proj
      (let* ((dirs (unless (search-scope-root-p scope)
                     (list absolute)))
             (files (mapcar #'(lambda (path)
                                (file-relative-name path absolute))
                            (project-files proj dirs)))
             (list (if exclude-regexp
                       (seq-remove
                        (apply-partially #'string-match-p exclude-regexp)
                        files)
                     files)))
        (cond
         ((consp regexp)
          (mapcan
           #'(lambda (regexp)
               (seq-filter (apply-partially #'string-match-p regexp)
                           list))
           regexp))
         ((stringp regexp)
          (seq-filter (apply-partially #'string-match-p regexp)
                      list))
         (t list))))))

(defun search-scope-index-files (scope &optional regexp exclude-related-scopes)
  "Returns a list of files located inside of SCOPE using indexers from the
`search-scope-indexers'. List contains relative paths filtered
using REGEXP. REGEXP can be a string or a list of strings. if
EXCLUDE-RELATED-SCOPES is non nil then related scope direcotries
are excluded."

  (let* ((relatives (mapcar
                     #'(lambda (scope)
                         (alist-get 'relative scope))
                     (search-scope-related-scopes scope)))
         (exclude (when (and (search-scope-root-p scope)
                             exclude-related-scopes)
                    (delete nil relatives))))
    (seq-some #'(lambda (fun)
                  (funcall fun scope exclude regexp))
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

(defun search-scope-construct (root &optional path strategy)
  "Constucts a new scope from an existing one or from scratch. ROOT can be
alist (an existing scope) or string (absolute path). PATH is a relative
or an absolute path."

  (cl-assert
   (memq (type-of root) '(cons string)))
  (let* ((scope (if (consp root)
                    (assoc-delete-all 'relative (copy-alist root))
                  `((root . ,root))))
         (dir (when path (file-name-directory path)))
         (root (alist-get 'root scope)))
    (cond
     ((and dir (file-name-absolute-p dir))
      (let ((relative (file-relative-name dir root)))
        (setf (alist-get 'absolute scope) dir
              (alist-get 'strategy scope) (or strategy 'default))
        (push `(relative . ,relative) scope)))
     (dir
      (let ((absolute (expand-file-name dir root)))
        (setf (alist-get 'absolute scope) absolute
              (alist-get 'strategy scope) (or strategy 'default))
        (push `(relative . ,dir) scope)))
     (t
      (setf (alist-get 'absolute scope) root
            (alist-get 'strategy scope)
            (or strategy search-scope-root-strategy))))
    scope))

(defmacro search-scope-root-p (scope)
  `(null (alist-get 'relative ,scope)))

(defmacro search-scope-root-equal (scope peer)
  `(string= (alist-get 'root ,scope)
            (alist-get 'root ,peer)))

(defun search-scope-greater-p (scope peer)
  (let ((same-roots (search-scope-root-equal scope peer)))
    (if same-roots
        (string> (alist-get 'absolute scope)
                 (alist-get 'absolute peer))
      (string> (alist-get 'root scope)
               (alist-get 'root peer)))))

(defun search-scope-add (scope list)
  "adds SCOPE to LIST and sorts using `search-scope-greater-p'"
  (seq-sort #'search-scope-greater-p
            (push scope list)))

(defun search-scope-absolute-path (scope &optional abbreviate)
  "Returns an absolute directory path of SCOPE that is abbreviated
if ABBREVIATE is non nil."

  (let ((absolute (alist-get 'absolute scope)))
    (if abbreviate
        (abbreviate-file-name absolute)
      absolute)))

(defmacro search-scope-relative-name (scope path)
  `(file-relative-name ,path (alist-get 'absolute ,scope)))

(defmacro search-scope-expand-name (scope path)
  `(expand-file-name ,path (alist-get 'absolute ,scope)))

(defun search-scope-marked-dirs (scope)
  "looks for marked direcotries in SCOPE."

  (let ((dirs (mapcar #'file-name-directory
                      (search-scope-index-files
                       scope
                       search-scope-marker-regexps))))
    (delete nil (delete-dups dirs))))

(defun search-scope-find-pairs (scope)
  "looks for file pairs in SCOPE"

  (let ((files (search-scope-index-files scope nil t))
        (names (make-hash-table :test #'equal))
        (pairs))
    (dolist (file files)
      (let* ((base (file-name-base file))
             (list (gethash base names)))
        (puthash base (cons file list) names)))
    (maphash #'(lambda (name list)
                 (when (length> list 1)
                   (push (cons name (seq-sort #'string> list))
                         pairs)))
             names)
    pairs))

(defun search-scope-discover-scopes (dir)
  "Discovers scopes in a root directory DIR using
`search-scope-marked-dirs'."

  (cl-assert (and
              (file-name-absolute-p dir)
              (file-directory-p dir)))
  (let* ((root (search-scope-construct dir))
         (list (list root)))
    (nconc list
           (mapcar #'(lambda (dir)
                       (search-scope-construct root dir))
                   (search-scope-marked-dirs root)))))

(defun search-scope-find (dir scopes)
  "Finds a scope in SCOPES list by absolute, relative or root path. DIR
type can be cons or string. If DIR type is string then it is absolute
directory path."

  (cl-assert
   (memq (type-of dir) '(cons string)))
  (let ((type (if (consp dir) (car dir) 'absolute))
        (path (if (consp dir) (cdr dir) dir)))
    (cl-assert
     (memq type '(root absolute relative)))
    (seq-find #'(lambda (scope)
                  (string= (alist-get type scope)
                           path))
              (if (eq type 'root)
                  (seq-filter #'(lambda (scope)
                                  (search-scope-root-p scope))
                              scopes)
                scopes))))

(defun search-scope-closest (path scopes)
  "finds PATH closest scope from SCOPES list."

  (seq-reduce
   #'(lambda (closest scope)
       (let ((closest-dir (alist-get 'absolute closest))
             (dir (alist-get 'absolute scope)))
         (if (and (string-match-p (regexp-quote dir) path)
                  (or (null closest)
                      (string> dir closest-dir)))
             scope
           closest)))
   scopes
   '()))

(defun search-scope-related-scopes (original &optional list)
  "finds all related scopes in `search-scope-list'."

  (seq-reduce #'(lambda (accum scope)
                  (cond
                   ((not (search-scope-root-equal original scope))
                    accum)
                   ((search-scope-find
                     (assoc 'absolute scope)
                     accum)
                    accum)
                   (t
                    (push scope accum))))
              (or list search-scope-list)
              `(,original)))

(defun search-scope-get (dir)

  (cl-assert (and
              (file-name-absolute-p dir)
              (file-directory-p dir)))
  (let* ((root-dir (search-scope-discover-root dir dir))
         (root (search-scope-find (cons 'root root-dir)
                                  search-scope-list)))
    (when root
      (let ((scopes (search-scope-related-scopes root)))
        (search-scope-closest dir scopes)))))

(defun search-scope-discover (dir)

  (cl-assert (and
              (file-name-absolute-p dir)
              (file-directory-p dir)))
  (let* ((root-dir (search-scope-discover-root dir dir))
         (scopes (search-scope-discover-scopes root-dir))
         (scope (search-scope-closest dir scopes)))
    (setf search-scope-list
          (seq-sort #'search-scope-greater-p
                    (nconc search-scope-list scopes)))
    (message "Root scope is discovered in %s"
             (search-scope-absolute-path scope t))
    scope))

;;;###autoload
(defun search-scope-link-buffer (&optional buffer relink)
  "Links BUFFER to an existing scope from `search-scope-list'."

  (let ((fun
         #'(lambda (dir)
             (let ((scope (if (or (null search-scope)
                                  relink)
                              (search-scope-get dir)
                            search-scope)))
               (search-scope-mode (if (null search-scope) -1 1))
               (setf search-scope (copy-alist scope))))))
    (if buffer
        (with-current-buffer buffer
          (funcall fun (expand-file-name default-directory)))
      (funcall fun (expand-file-name default-directory)))))

(defun search-scope-require-scope (&optional relink)
  "Finds a closest scope of the current buffer. If one does not
exist then it discovers a root directory and creates a list of
related scopes. Closest scope is saved to`search-scope' buffer
local variable."

  (if (and search-scope
           (null relink))
      search-scope
    (let ((scope (search-scope-link-buffer (current-buffer) relink)))
      (if scope
          scope
        (let* ((dir (expand-file-name default-directory))
               (scope (search-scope-discover dir)))
          (search-scope-mode 1)
          (setf search-scope (copy-alist scope)))))))

(defun search-scope-read-adjust (scope &optional path)
  "Reads a scope from a list of related scopes of SCOPE and adjusts
one according to user input."

  (let* ((original (search-scope-completing-read-related scope))
         (scope (copy-alist original))
         (strategy (search-scope-read-strategy scope)))
    (unless (search-scope-root-p original)
      (setf
       (alist-get 'strategy original) strategy))
    (setf
     (alist-get 'strategy scope) strategy)
    (when (eq strategy 'same-files)
      (if path
          (push `(ext . ,(file-name-extension path))
                scope)
        (push `(ext . ,(read-string "File extension: "))
              scope)))
    scope))

(defun search-scope-completing-read-related (scope &optional prompt)
  "Reads a related scope with completion. Candidate list is built by
`search-scope-related-scopes'"

  (let ((root (alist-get 'root scope))
        (related (search-scope-related-scopes scope)))
    (if (length> related 0)
        (let* ((dirs (mapcar #'(lambda (scope)
                                 (alist-get 'relative scope))
                             related))
               (dir (completing-read
                     (or prompt
                         (format "Scope (%s): "
                                 (abbreviate-file-name root)))
                     (delete nil dirs) nil t nil nil "./")))
          (search-scope-find (expand-file-name dir root) related))
      (list scope))))

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

(defun search-scope-read-thing (&optional prompt)
  "Reads a thing from minibuffer"

  (let* ((search-scope-history (search-scope-simple-history))
         (name (completing-read
                (or prompt
                    "Specify a thing: ")
                search-scope-history
                nil nil nil
                'search-scope-history)))
    (let ((thing (search-scope-pull-from-history name)))
      (if thing
          thing
        (search-scope-add-to-history (cons name 'string))))))

(defun search-scope-add-to-history (thing)
  "records THING to `search-scope-searched-things'"

  (let ((alist search-scope-searched-things)
        (length (length search-scope-searched-things)))
    (when (>= length search-scope-max-history-size)
      (setf alist (nbutlast alist (- (1+ length)
                                     search-scope-max-history-size))))
    (setf search-scope-searched-things
          (assoc-delete-all (car thing) alist)))
  (push thing search-scope-searched-things)
  thing)

(defun search-scope-pull-from-history (name)
  "Lookups a thing in `search-scope-searched-things' by
NAME. `search-scope-searched-things' is rearranged in case of successful
lookup."

  (let* ((alist search-scope-searched-things)
         (thing (assoc name alist)))
    (when thing
      (setf search-scope-searched-things
            (cons thing (delete thing alist))))
    thing))

(defun search-scope-simple-history (&optional omit-first-thing)
  "It transforms `search-scope-searched-things' to a simple list of
thing names"

  (let ((names (mapcar #'car search-scope-searched-things)))
    (if omit-first-thing
        (cdr names)
      names)))

(defun search-scope-highlight (bounds seconds)
  "Highlights a region with BOUNDS in a current buffer. BOUNDS can
be a cons or a list."

  (let* ((beg (car bounds))
         (end (cdr bounds))
         (overlay (make-overlay beg (if (consp end) (car end) end)))
         (destructor (apply-partially #'delete-overlay overlay)))
    (overlay-put overlay 'face 'region)
    (overlay-put overlay 'evaporate t)
    (when (natnump seconds)
      (run-at-time seconds nil destructor))
    overlay))

(defun search-scope-get-thing (thing-type &optional trim no-highlight)
  "returns thing at point as `cons'"

  (let ((bounds (bounds-of-thing-at-point thing-type)))
    (when (and (null no-highlight)
               search-scope-thing-flash-seconds)
      (search-scope-highlight bounds
                              search-scope-thing-flash-seconds))
    (cons
     (let ((string (buffer-substring-no-properties
                    (car bounds)
                    (cdr bounds))))
       (if trim
           (string-trim string trim trim)
         string))
     thing-type)))

(defun search-scope-get-contextual-thing (buffer &optional no-highlight)
  "Looks up for a thing in buffer BUFFER depending on major mode and
an active mark state."

  (with-current-buffer buffer
    (cond
     ;; check region at first in case of only part of a thing at point
     ;; should be searched.
     ((use-region-p)
      (let ((substring (buffer-substring-no-properties
                        (region-beginning) (region-end)))
            (deactivate-mark 'dont-save))
        (deactivate-mark)
        (cons substring 'string)))

     ((thing-at-point 'email)
      (search-scope-get-thing 'email "[<>]" no-highlight))

     ((thing-at-point 'uuid)
      (search-scope-get-thing 'uuid nil no-highlight))

     ((and (derived-mode-p 'dired-mode)
           (thing-at-point 'filename))
      (search-scope-get-thing 'filename nil no-highlight))

     ;; `apply' is for compatibility reasons
     ((and (apply #'derived-mode-p search-scope-symbolic-modes)
           (thing-at-point 'symbol))
      (search-scope-get-thing 'symbol nil no-highlight))

     ((and (derived-mode-p 'text-mode)
           (thing-at-point 'word))
      (search-scope-get-thing 'word nil no-highlight)))))

(defun search-scope-thing-at-point (&optional no-history no-highlight)
  "Picks up a thing at point or a literal if there is an active
region. If NO-HISTORY is nil then new thing is added to
`search-scope-searched-things'."

  (let ((thing (search-scope-get-contextual-thing
                (current-buffer)
                no-highlight)))
    (if (and thing (null no-history))
        (search-scope-add-to-history thing)
      thing)))

(defun search-scope-thing-to-regexp (thing &optional trim-word)
  "Creates a regexp from THING. If TRIM-WORD is not nil then word
ending is trimmed by `search-scope-trim-word'."

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

(defun search-scope-thing-positions (thing buffer &optional limit)
  "Searches at most LIMIT positions of THING in a buffer BUFFER."

  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))

      (let ((regexp (search-scope-thing-to-regexp thing))
            (case-fold-search nil)
            (list))
        (while (and (re-search-forward regexp nil t)
                    (if limit (length< list limit) t))
          (let ((string (substring-no-properties (match-string 0)))
                (beg (match-beginning 0))
                (end (match-end 0))
                (line-beg (line-beginning-position)))
            (push (list string beg end line-beg) list)))
        (nreverse list)))))

;;;###autoload
(defun search-scope-replace (thing new-name)
  "it renames THING to NEW-NAME in a current buffer using
`query-replace-regexp'"

  (interactive
   (let ((thing (search-scope-thing-at-point))
         (search-scope-history (search-scope-simple-history t)))
     (unless thing
       (user-error "There is nothing at point"))
     (list thing
           (read-string (format "Rename '%s' to: " (car thing))
                        (car thing) 'search-scope-history nil t))))

  (cond
   ((buffer-modified-p)
    (user-error "Rename of '%s' is aborted since buffer '%s' is modified"
                (car thing) (buffer-name (current-buffer))))
   (t
    (save-excursion
      (let ((thing-regexp (cond ((search-scope-is thing 'symbol)
                                 (format "\\_<%s\\_>" (car thing)))
                                ((search-scope-is thing 'word)
                                 (format "\\b%s" (car thing)))
                                (t (format "\\b%s\\b" (search-scope-quote thing))))))
        (goto-char (point-min))
        (query-replace-regexp thing-regexp new-name))))))

;;;###autoload
(defun search-scope-grep (thing scope)
  "searches THING in SCOPE using an engine from
`search-scope-grep-engines' alist. If prefix argument is not nil then
user is queried to adjust scope and searching strategy. `search-scope'
is updated with a new value."

  (interactive
   (list
    ;; double universal prefix argument
    (if (> (prefix-numeric-value current-prefix-arg) 1)
        (search-scope-read-thing)
      (or (search-scope-thing-at-point)
          (search-scope-read-thing)))
    (if current-prefix-arg
        (search-scope-read-adjust
         (search-scope-require-scope)
         (buffer-file-name))
      (search-scope-require-scope))))

  (let* ((engine (search-scope-get-engine))
         (args (funcall (cdr engine)
                        thing
                        scope)))
    (setf search-scope scope)
    ;; special variables to be overridden since they affect execution context
    (let ((default-directory (alist-get 'root scope))
          (current-prefix-arg nil))
      (apply (car engine) args))))

;;;###autoload
(defun search-scope-remember (buffer)
  "Constructs a new scope in `default-directory' of BUFFER and
adds one to `search-scope-list'"

  (interactive (list (current-buffer)))

  (with-current-buffer buffer
    (let ((dir (expand-file-name default-directory))
          (scope (search-scope-require-scope)))
      (cond
       ((null scope)
        (let ((scope (search-scope-construct dir)))
          (setf search-scope-list
                (search-scope-add scope search-scope-list))
          (setf search-scope scope))
        (search-scope-mode 1))
       ((not (search-scope-find dir search-scope-list))
        (let ((scope (search-scope-construct scope dir)))
          (setf search-scope-list
                (search-scope-add scope search-scope-list))
          (setf search-scope scope))
        (search-scope-mode 1))
       (t
        (user-error "%s is already in scope" (abbreviate-file-name dir)))))))

(defun search-scope-circ-next (elt list)
  "finds an element that is placed after ELT."

  (seq-reduce #'(lambda (accum elt_)
                  (cond
                   ((string= accum elt_) nil)
                   ((null accum) elt_)
                   (t accum)))
              (append (last list) list)
              elt))

;;;###autoload
(defun search-scope-find-pair (scope path &optional thing no-cache)
  "Looks up for a pair to find one and display buffer using
`search-scope-display-pair-function'."

  (interactive (list (search-scope-require-scope)
                     (or (buffer-file-name)
                         (user-error "current buffer has no visited file"))
                     (search-scope-get-contextual-thing (current-buffer) t)
                     current-prefix-arg))

  (let ((relative (search-scope-relative-name scope path))
        (pair (alist-get 'pair scope)))
    (when (or (null pair ) no-cache)
      (let ((pairs (search-scope-find-pairs scope))
            (base (file-name-base relative)))
        (setf pair (assoc-default base pairs))
        (if pair
            (setf (alist-get 'pair scope) pair)
          (user-error "no pair for %s" relative))))
    ;; save scope after modification
    (setf search-scope scope)
    (setf relative (search-scope-circ-next relative pair))
    (cl-assert relative)
    (let* ((absolute (search-scope-expand-name scope relative))
           (buffer (or (get-file-buffer absolute)
                       (find-file-noselect absolute)))
           (other-thing (when thing (search-scope-get-contextual-thing buffer t)))
           (action (cons search-scope-display-pair-function
                         '((inhibit-switch-frame . t)))))
      (let ((w (display-buffer buffer action))
            (bounds (when (and thing
                               (not (equal thing other-thing)))
                      (cdar (search-scope-thing-positions thing buffer 1)))))
        (when bounds
          (search-scope-add-to-history thing)
          (set-window-point w (car bounds))
          (when search-scope-thing-flash-seconds
            (with-current-buffer buffer
              (search-scope-highlight bounds
                                      search-scope-thing-flash-seconds))))
        (select-window w)))))

(provide 'search-scope-mode)
