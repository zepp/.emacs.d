(setq
 calendar-week-start-day 1
 default-input-method 'russian-computer
 custom-file (locate-user-emacs-file "custom.el"))

;; prevent warning buffer from popping up in case of
;; `undo-outer-limit' is exceeded
(require 'warnings)
(add-to-list 'warning-suppress-types '(undo discard-info))

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      uniquify-ignore-buffers-re "^\\*")

(setq switch-to-buffer-obey-display-actions t)

;; some of this modes provide navigation capabilities
(add-to-list 'display-buffer-alist
             '((or (derived-mode . compilation-mode)
                   (major-mode . occur-mode)
                   (major-mode . xref--xref-buffer-mode))
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (side . bottom)
               (window-min-height . 0.25)))

(setq completion-styles '(basic substring)
      completion-show-help nil
      completion-auto-select 'second-tab
      completion-auto-help 'visible
      completion-ignore-case t
      ;; minibuffer
      completions-format 'one-column
      completions-max-height 10)

(add-hook 'kill-emacs-hook #'basic-save-buffer)

;;-------------------------------------------------------------------------------
;; package management

(let ((default-directory "~/.emacs.d/loadable/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(setq  use-package-always-defer t
       use-package-hook-name-suffix nil)
(require 'package)
(setq use-package-compute-statistics t)
(add-to-list
 'package-archives
 '("melpa" . "http://stable.melpa.org/packages/")
 t)
(package-initialize)

(require 'pavel-compat)
(require 'pavel-commands)
;;-------------------------------------------------------------------------------

(defun form-shell-command-buffer-name (orig &rest args)
  "Changes output buffer name to a command name"

  (let* ((command (nth 0 args))
         (shell-command-buffer-name-async (format "*async: %s*" command)))
    (apply orig args)))

(advice-add 'async-shell-command :around #'form-shell-command-buffer-name)

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

(defun pavel/eshell-jump ()
  "it starts eshell in a current directory or switches buffer to
existing one"
  (interactive)

  (let* ((eshell-buffer-name (pavel/eshell-buf-name))
         (buf (get-buffer eshell-buffer-name)))
    (if buf
        (display-buffer buf '(display-buffer-same-window))
      (eshell))))

(use-package dired
  :bind (:map dired-mode-map
              ("C-t C-t" . nil)
              ("c" . nil)
              ("$" . pavel/eshell-jump)
              ("z" . dired-do-compress-to)
              ("TAB" . dired-hide-subdir)
              ("M-*" . dired-mark-files-regexp))

  :init
  (setq dired-deletion-confirmer #'y-or-n-p
        ;; -l is mandatory
        ;; -G omit the group name
        ;; -h human-readable size
        dired-listing-switches
        (if (string= system-type "darwin")
            "-aloh"
          "-alGh"))

  :config
  (cond
   ((string= system-type "gnu/linux")
    (add-to-list 'dired-guess-shell-alist-user '("\\.pdf$" "xdg-open"))
    (add-to-list 'dired-guess-shell-alist-user '("\\.odt$" "xdg-open")))))

(use-package dired-aux
  :config
  (add-to-list 'dired-compress-files-alist
               '("\\.7z$" . "7z a %o %i")))

(use-package shell
  :hook
  (shell-mode-hook . shell-dirtrack-mode))

(defun pavel/rename-eshell-buf()
  "hook that keeps eshell buffer name actual"

  (let* ((name (pavel/eshell-buf-name))
         (buf (get-buffer name)))
    (unless buf
      (rename-buffer name))))

(use-package eshell
  :hook
  (eshell-directory-change-hook . pavel/rename-eshell-buf))

(defun pavel/compile-buf-name (orig &rest args)
  "prettify name of compilation buffer"

  (let* ((compilation-buffer-name-function
          #'(lambda (mode) (format "*%s: %s*" mode (nth 0 args)))))
    (apply orig args)))

(use-package compile
  :config
  (let ((alist
         '((tsl
            "^\\[tsl\\]\\s-+ERROR in \\([^(\r\n]+\\)(\\([0-9]+\\),\\([0-9]+\\))?$" 1 2 3)
           (angular-nx
            "^\\s-*\\([^:\r\n]+\\):\\([0-9]+\\):\\([0-9]+\\):$" 1 2 3))))
    (dolist (cell alist)
      (add-to-list 'compilation-error-regexp-alist-alist cell)
      (add-to-list 'compilation-error-regexp-alist (car cell))))

  (advice-add 'compile :around #'pavel/compile-buf-name))

;;-------------------------------------------------------------------------------
;; UI related

(setq
 inhibit-startup-screen t
 frame-title-format "%b"
 visible-bell t
 make-pointer-invisible t
 x-select-enable-clipboard t
 visual-line-fringe-indicators '(nil right-curly-arrow))

(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))

(tool-bar-mode -1)
(toggle-scroll-bar -1)
(column-number-mode 1)

(defun pavel/tab-bar-map ()
  "builds custom `tab-bar-mode' map"

  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-t") #'tab-bar-new-tab)
    (define-key map (kbd "C-f") #'tab-bar-switch-to-next-tab)
    (define-key map (kbd "C-b") #'tab-bar-switch-to-prev-tab)
    (define-key map (kbd "C-r") #'tab-bar-switch-to-recent-tab)
    (define-key map (kbd "C-l") #'tab-bar-switch-to-last-tab)
    (define-key map (kbd "C-w") #'tab-bar-close-tab)
    map))

(use-package tab-bar
  :bind-keymap ("C-t" . tab-bar-mode-map)

  :config
  (setq tab-bar-mode-map (pavel/tab-bar-map)
        tab-bar-new-tab-choice "*scratch*")
  (tab-bar-mode 1)

  :demand t)

(use-package which-key
  :init (setq which-key-idle-delay 2.0)
  :config (which-key-mode 1)
  :demand t
  :ensure t)

(use-package ido
  :bind ("C-x C-d" . ido-dired)
  :init
  (setq ido-enable-flex-matching t
        ido-cannot-complete-command 'ido-next-match
        ido-use-virtual-buffers t)

  :config
  (dolist (buf '("\\*Quail Completions\\*"
                 "\\*Completions\\*"
                 "\\*Buffer List\\*"))
    (add-to-list 'ido-ignore-buffers buf))

  (ido-mode 1)

  :demand t)

(use-package isearch
  :bind
  (:map isearch-mode-map
        ("M-o" . isearch-occur))
  (:map search-map
        ("s" . isearch-forward-thing-at-point))
  :init
  (setq isearch-allow-motion t
        isearch-motion-changes-direction t)

  :demand t)

;;-------------------------------------------------------------------------------

;; disable tab indentation globally
(setq-default indent-tabs-mode nil)

(save-place-mode 1)
(transient-mark-mode 1)
(delete-selection-mode 1)
(electric-pair-mode 1)

(use-package whitespace
  :init (setq whitespace-style '(face
                                 trailing
                                 space-after-tab
                                 space-before-tab)))

(defun pavel/ws-long-lines ()
  "highlights long lines using whitespace-mode"

  (setq-local whitespace-style (cons 'lines-char whitespace-style)))

;;-------------------------------------------------------------------------------
;; programming modes

(use-package prog-mode
  :hook
  (prog-mode-hook . visual-line-mode)
  (prog-mode-hook . whitespace-mode)
  (prog-mode-hook . auto-revert-mode))

(use-package elisp-mode
  :hook (emacs-lisp-mode-hook . eldoc-mode))

(use-package company
  :after prog-mode
  :bind (:map company-mode-map
              ("C-M-i" . company-complete))
  :hook (prog-mode-hook . company-mode)
  :ensure t)

(use-package tree-sitter-langs
  :ensure t
  :demand t)

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

(use-package treesit
  :mode
  (("\\.css\\'" . css-ts-mode)
   ("\\.scss\\'" . css-ts-mode)
   ("\\.tsx\\'" . tsx-ts-mode)
   ("\\.jsx\\'" . tsx-ts-mode)
   ("\\.js\\'"  . typescript-ts-mode)
   ("\\.ts\\'"  . typescript-ts-mode)
   ("\\.mjs\\'" . typescript-ts-mode)
   ("\\.mts\\'" . typescript-ts-mode)
   ("\\.cjs\\'" . typescript-ts-mode)
   ("\\.json\\'" .  json-ts-mode)
   ("\\.py\\'" . python-ts-mode))

  :init
  (dolist (mapping
           '((css-mode . css-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (js-mode . typescript-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)
             (python-mode . python-ts-mode)
             (bash-mode . bash-ts-mode)
             (sh-mode . bash-ts-mode)
             (sh-base-mode . bash-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping)))

(use-package project
  :bind-keymap ("C-x C-p" . project-prefix-map)
  :bind (:map project-prefix-map
              ("C-f" . project-find-file)
              ("C-d" . project-find-dir)
              ("C-j" . project-dired)
              ("$" . project-eshell)))

(use-package ag
  :after prog-mode
  :bind (:map prog-mode-map
              ("C-c g" . ag-project-regexp)
              ("C-c M-g" . ag-project-files))
  :init
  (setq ag-reuse-buffers t)

  :ensure t)

;;-------------------------------------------------------------------------------
;; version control

(use-package diff-mode
  :bind (:map diff-mode-map
              ("C-M-n" . diff-file-next)
              ("C-M-p" . diff-file-prev)
              ("C-j" . diff-goto-source)))

(use-package magit
  :after ido
  :bind (("C-x M-v" . magit-file-dispatch)
         (:map magit-diff-mode-map
               ("C-M-n" . magit-section-forward-sibling)
               ("C-M-p" . magit-section-backward-sibling)))

  :init
  (setq magit-display-buffer-function
        'magit-display-buffer-fullframe-status-v1)

  :config
  ;; append to end of hook list
  (add-hook 'git-commit-setup-hook 'turn-off-auto-fill t)
  (add-hook 'git-commit-setup-hook #'pavel/ws-long-lines)
  (add-to-list 'ido-ignore-buffers "^magit-process: .*")
  (add-to-list 'ido-ignore-buffers ".*\\.~[[:alnum:]]+~$")
  (add-to-list 'display-buffer-alist
               '((derived-mode . magit-diff-mode)
                 (display-buffer-reuse-mode-window
                  display-buffer-pop-up-window)
                 (dedicated . t)))

  :ensure t)

(use-package ssh-agency
  :demand t
  :ensure t)

;;-------------------------------------------------------------------------------

(use-package abbrev
  :bind (:map edit-abbrevs-mode-map
              ("C-x C-w" . nil)))

(use-package ispell
  :init
  (setq ispell-program-name "hunspell"
        ispell-dictionary "ru_RU,en_US"
        ispell-personal-dictionary "~/.hunspell_personal")

  :config
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "ru_RU,en_US")
  (unless (file-exists-p ispell-personal-dictionary)
    (write-region "" nil ispell-personal-dictionary nil 0)))

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

(use-package text-mode
  :bind (:map text-mode-map
              ("M-q" . pavel/double-q-marks)
              ("C-M--" . pavel/insert-dash)
              ("C-c C-q" . fill-paragraph)
              ("C-c C-o" . browse-url))
  :hook
  (text-mode-hook . visual-line-mode)
  (text-mode-hook . whitespace-mode)
  (text-mode-hook . flyspell-mode)
  (text-mode-hook . abbrev-mode))

(use-package visual-fill-column
  :ensure t)

(defun pavel/setup-org-directory (directory)
  "it setups Org-mode's root directory and agenda files"
  (interactive "D")

  (setq
   org-directory directory
   org-agenda-files `(,directory)
   org-default-notes-file (expand-file-name "default.org" directory)
   org-archive-location (expand-file-name "archive.org::" directory)))

(use-package org-agenda
  :bind (("C-x C-a" . org-agenda)
         (:map org-agenda-mode-map
               ("n" . org-agenda-next-item)
               ("p" . org-agenda-previous-item)
               ("C-M-n" . org-agenda-next-date-line)
               ("C-M-p" . org-agenda-previous-date-line)
               ("C-x C-w" . nil) ;; org-agenda-write
               ("C-c C-w" . nil) ;; org-agenda-refile
               ("C-c C-r" . org-agenda-refile)
               ("C-c C-p" . org-agenda-set-property)
               ("C-x C-n" . org-agenda-capture)))
  :init
  (setq
   org-agenda-remove-tags t
   org-agenda-window-setup 'only-window))

(use-package org
  :bind (("C-x C-n" . org-capture)
         (:map org-mode-map
               ("M-m" . org-emphasize)
               ("C-M-f" . org-forward-element)
               ("C-M-b" . org-backward-element)
               ("C-M-u" . org-up-element)
               ("C-M-d" . org-down-element)
               ("C-M-SPC" . org-mark-element)
               ("C-M-n" . org-next-visible-heading)
               ("C-M-p" . org-previous-visible-heading)
               ("C-c C-w" . org-cut-special)   ;; org-refile
               ("C-c M-w" . org-copy-special)  ;; org-refile-copy
               ("C-c C-y" . org-paste-special) ;; org-evaluate-time-range
               ("C-c C-r" . org-refile)        ;; org-reveal
               ("C-c M-r" . org-refile-copy)
               ("C-c C-p" . org-set-property)
               ("C-c C-b" . org-mark-ring-goto)
               ("C-c C-x C-r" . org-reveal)       ;; org-toggle-radio-button
               ("C-c C-x C-s" . org-store-link))) ;; org-archive-subtree

  :init
  (setq
   org-hide-emphasis-markers t
   org-tags-column 0
   org-log-into-drawer t
   org-fold-catch-invisible-edits 'show-and-error
   org-time-stamp-custom-formats '("<%a %d %b %Y>" . "<%a %H:%M %d %b %Y>")
   org-archive-subtree-save-file-p t
   org-goto-auto-isearch nil

   org-export-with-toc nil
   org-export-with-section-numbers nil
   org-export-initial-scope 'subtree)

  (add-to-list 'display-buffer-alist
               '((or
                  ;; there is a whitespace at the beginning of the buffer name
                  "\\*Agenda Commands\\*$"
                  "\\*Org Export Dispatcher\\*$"
                  "\\*Org Select\\*$"
                  "\\*Org Attach\\*$"
                  "\\*Org Links\\*$"
                  "\\*Org Help\\*$"
                  "\\*Select Link\\*$"
                  (major-mode . calendar-mode))
                 display-buffer-at-bottom
                 (dedicated . t)))

  (add-to-list 'display-buffer-alist
               '("CAPTURE-\\w+\\.org$"
                 display-buffer-pop-up-window
                 (dedicated . t)))
  :config
  (add-to-list 'org-latex-packages-alist '("AUTO" "babel" t ("pdflatex")))
  (add-to-list 'org-latex-packages-alist '("" "tabularx"))
  (add-to-list 'org-latex-packages-alist '("" "spverbatim"))

  :hook
  (org-mode-hook . org-indent-mode)
  (org-mode-hook . org-toggle-time-stamp-overlays)
  (org-mode-hook . auto-revert-mode)
  (org-mode-hook . flyspell-mode)

  :mode
  (("\\.org\\'" . org-mode)))

(use-package ox-html
  :after org
  :defer t
  :init
  (setq
   ;; highlight.js to be used
   org-html-htmlize-output-type nil
   org-html-self-link-headlines t
   org-html-head-include-default-style nil
   org-html-head-include-scripts nil
   org-html-postamble t)

  :config
  (add-to-list 'org-html-postamble-format
               `("ru"
                 ,(concat
                   "<p class=\"author\">%a</p>"
                   "<p class=\"timestamp\">%T</p>")))

  (add-to-list 'org-html-special-string-regexps
               '("—" . "&#x2014;"))
  (add-to-list 'org-html-special-string-regexps
               '("–" . "&#x2013;")))

;;-------------------------------------------------------------------------------

(keymap-global-set "C-c g" #'rgrep)
(define-key ctl-x-map (kbd "M-s") #'write-file)
(define-key ctl-x-map (kbd "M-b") #'switch-to-buffer-other-window)
(define-key ctl-x-map (kbd "M-f") #'find-file-other-window)
(define-key ctl-x-map (kbd "M-d") #'dired-other-window)
(define-key ctl-x-map (kbd "M-j") #'dired-jump-other-window)
(when (string-match "29\\.[0-9]+" emacs-version)
  (define-key ctl-x-map (kbd "w d") #'zeppa/toggle-window-dedicated))
;; originaly it was `write-file'
(define-key ctl-x-map (kbd "C-w") #'quit-window)
;; originally it was prefix key for project keymap
(define-key ctl-x-map (kbd "p") #'previous-buffer)
(define-key ctl-x-map (kbd "$") #'pavel/eshell-jump)

;;-------------------------------------------------------------------------------
;; os specific configuration

(cond

 ((string= system-type "windows-nt")
  ;; it looks ugly
  (menu-bar-mode -1)

  (setq
   ;; make `rgrep' work
   find-program
   (expand-file-name "bin/find.exe"
                     (getenv "ChocolateyInstall")))

  (with-eval-after-load 'grep
    (setq grep-mode-font-lock-keywords
          (pavel/fix-find-regexp grep-mode-font-lock-keywords)))

  ;; make `hunspell' work
  (let ((root (expand-file-name ".dicts"
                                (or (getenv "HOME")
                                    (getenv "HOMEPATH")))))
    (setenv "DICPATH" root)
    (setenv "DICTIONARY" "en_US")
    (setq ispell-hunspell-dict-paths-alist (pavel/list-dicts root))))

 ((string= system-type "darwin")
  (setq mac-command-modifier 'meta
        mac-option-modifier 'none
        browse-url-browser-function 'browse-url-default-macosx-browser
        visible-bell nil)
  (when (eq window-system 'ns)
    (use-package exec-path-from-shell
      :config
      (add-to-list 'exec-path-from-shell-variables "LANG")
      (exec-path-from-shell-initialize)
      :demand t
      :ensure t))))

;;-------------------------------------------------------------------------------

(load custom-file :noerror)
