(setq
 calendar-week-start-day 1
 default-input-method 'russian-computer
 custom-file "~/.emacs.d/custom.el"
 use-package-hook-name-suffix nil)

;; prevent warning buffer from popping up in case of
;; `undo-outer-limit' is exceeded
(require 'warnings)
(add-to-list 'warning-suppress-types '(undo discard-info))

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      uniquify-ignore-buffers-re "^\\*")

(add-to-list 'same-window-buffer-names "*grep*")

(add-hook 'kill-emacs-hook #'basic-save-buffer)

;;-------------------------------------------------------------------------------
;; package management

(let ((default-directory "~/.emacs.d/loadable/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(require 'package)
(add-to-list
 'package-archives
 '("melpa" . "http://stable.melpa.org/packages/")
 t)
(package-initialize)

;;-------------------------------------------------------------------------------

(defun form-shell-command-buffer-name (orig &rest args)
  "Changes output buffer name to a command name"

  (let* ((command (nth 0 args))
         (shell-command-buffer-name-async (format "*async: %s*" command)))
    (apply orig args)))

(advice-add 'async-shell-command :around #'form-shell-command-buffer-name)

(defun dired-shell-command-popup (orig &rest args)
  "Controls the fashion of window splitting. Splits window
vertically."
  (let ((split-height-threshold 0)
        (split-width-threshold nil))
    (apply orig args)))

(use-package dired
  :bind (:map dired-mode-map
              (("c" . dired-do-copy)
               ("d" . dired-do-delete)
               ("r" . dired-do-rename)
               ("M-d" . dired-flag-file-deletion)))

  :init (setq dired-recursive-deletes 'always
              dired-deletion-confirmer #'y-or-n-p
              ;; -l is mandatory
              ;; -G omit the group name
              ;; -h human-readable size
              dired-listing-switches "-alGh")

  :config
  (advice-add 'dired-do-shell-command :around #'dired-shell-command-popup)

  :defer t)

(defun form-shell-buffer-name (orig &rest args)
  "it forms meaningful buffer name"

  (let* ((dir (directory-file-name default-directory))
         (name (concat (car (last (split-string dir "/")))
                       ":shell")))
    (apply orig
           (if (not (nth 0 args))
               (list name)
             args))))

(use-package shell
  :config
  (advice-add 'shell :around #'form-shell-buffer-name)
  :defer t)

(use-package ag
  :bind ("C-c g" . ag-regexp)
  :defer t
  :ensure t)

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

(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(tab-bar-mode 1)
(column-number-mode 1)

(use-package which-key
  :init (setq which-key-idle-delay 2.0)
  :config (which-key-mode 1)
  :ensure t)

(use-package ivy
  :bind (("C-c C-r" . ivy-resume)

         :map ivy-minibuffer-map
         ("M-t" . ivy-partial-or-done))

  :init (setq ivy-use-virtual-buffers t
              ivy-count-format "[%d/%d] ")

  :config
  (dolist (buf '("magit-process: .*"
                 "\\*vc-diff\\*.*"
                 "\\*tide-server\\*"
                 "\\*Quail Completions\\*"
                 "\\*Buffer List\\*"))
    (add-to-list 'ivy-ignore-buffers buf))
  (ivy-mode 1)

  :demand t
  :ensure t)

(use-package swiper
  :bind ("C-s" . swiper)
  :demand t
  :ensure t)

(use-package powerline
  :config (powerline-default-theme)
  :ensure t)

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

(defun zeppa/ws-long-lines ()
  "highlights long lines using whitespace-mode"

  (setq-local whitespace-style (cons 'lines-char whitespace-style)))

;;-------------------------------------------------------------------------------
;; programming modes

(use-package prog-mode
  :hook
  (prog-mode-hook . visual-line-mode)
  (prog-mode-hook . whitespace-mode))

(use-package elisp-mode
  :hook (emacs-lisp-mode-hook . eldoc-mode)
  :defer t)

(use-package company
  :bind ("M-t" . company-complete)
  :config (global-company-mode 1)
  :ensure t)

(use-package compile
  :init
  (setq
   compilation-auto-jump-to-first-error 'if-location-known
   compilation-scroll-output t)

  :config
  (let ((alist '((webpack "ERROR in \\([^(\r\n]+\\)(\\([0-9]+\\),\\([0-9]+\\))?$" 1 2 3))))
    (dolist (cell alist)
      (add-to-list 'compilation-error-regexp-alist-alist cell)
      (add-to-list 'compilation-error-regexp-alist (car cell))))

  :defer t)

(defun zeppa/ts-install-grammars ()
  (dolist (grammar
           '((css . ("https://github.com/tree-sitter/tree-sitter-css"))
             (html . ("https://github.com/tree-sitter/tree-sitter-html"))
             (json . ("https://github.com/tree-sitter/tree-sitter-json"))
             (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.23.1" "src"))
             (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "tsx/src"))
             (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "typescript/src"))
             (python . ("https://github.com/tree-sitter/tree-sitter-python"))))

    (add-to-list 'treesit-language-source-alist grammar)

    (unless (treesit-language-available-p (car grammar))
      (treesit-install-language-grammar (car grammar)))))

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
    (add-to-list 'major-mode-remap-alist mapping))

  :config
  (zeppa/ts-install-grammars))

;;-------------------------------------------------------------------------------
;; version control

(use-package diff-mode
  :bind (:map diff-mode-map
              ("C-m" . diff-goto-source))
  :defer t)

(use-package magit
  :config
  ;; append to end of hook list
  (add-hook 'git-commit-setup-hook 'turn-off-auto-fill t)
  (add-hook 'git-commit-setup-hook #'zeppa/ws-long-lines)

  :ensure t
  :defer t)

(use-package ssh-agency
  :demand t
  :ensure t)

;;-------------------------------------------------------------------------------
;; https://200ok.ch/posts/2020-08-22_setting_up_spell_checking_with_multiple_dictionaries.html

(use-package ispell
  :init
  (setq ispell-program-name "hunspell"
        ispell-dictionary "ru_RU,en_US"
        ispell-personal-dictionary "~/.hunspell_personal")

  :config
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "ru_RU,en_US")
  (unless (file-exists-p ispell-personal-dictionary)
    (write-region "" nil ispell-personal-dictionary nil 0))

  :defer t)

(defun zeppa/double-q-marks ()
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

(use-package text-mode
  :bind (:map text-mode-map
              ("C-q" . #'zeppa/double-q-marks)
              ("C-c C-o" . #'browse-url))
  :hook
  (text-mode-hook . visual-line-mode)
  (text-mode-hook . whitespace-mode)
  (text-mode-hook . flyspell-mode)
  (text-mode-hook . abbrev-mode)
  :defer t)

(use-package org
  :bind (("C-x C-a" . org-agenda)
         (:map org-mode-map
               ("C-c M-s" . #'org-store-link)))

  :init
  (setq
   org-hide-emphasis-markers t
   org-tags-column 0
   org-log-into-drawer t
   org-fold-catch-invisible-edits 'show-and-error
   org-time-stamp-custom-formats '("<%a %d %b %Y>" . "<%a %H:%M %d %b %Y>")
   org-agenda-remove-tags t
   org-agenda-window-setup 'current-window

   org-export-with-toc nil
   org-export-with-section-numbers nil
   org-export-initial-scope 'subtree
   ;; https://orgmode.org/manual/CSS-support.html
   org-html-self-link-headlines t
   org-html-head-include-default-style nil
   org-html-head-include-scripts nil
   org-html-htmlize-output-type nil
   org-html-head
   (concat "<link rel=\"stylesheet\" type=\"text/css\" href=\"dist/article.css\"/>\n"
           "<script src=\"dist/bundle.js\"></script>")
   org-html-head-extra "<meta property=\"og:type\" content=\"article\" />"
   org-html-postamble nil
   org-ascii-verbatim-format "«%s»"
   org-ascii-text-width 1000)

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
  (("\\.org\\'" . org-mode))

  :defer t)

;;-------------------------------------------------------------------------------

(global-set-key (kbd "C-x M-b") #'switch-to-buffer-other-window)
(global-set-key (kbd "C-x p") #'previous-buffer)
(global-set-key (kbd "C-x M-f") #'find-file-at-point)
(global-set-key (kbd "C-x C-x") #'server-edit)

;; window management in StumpWM style :)
(global-set-key (kbd "C-c s") #'split-window-horizontally)
(global-set-key (kbd "C-c v") #'split-window-vertically)
(global-set-key (kbd "C-c q") #'delete-other-windows)
(global-set-key (kbd "C-c k") #'delete-window)
(global-set-key (kbd "C-c o") #'other-window)

;;-------------------------------------------------------------------------------
;; os specific configuration

(defun zeppa/list-dicts(directory)
  "builds dictionary path alist for hunspell"

  (let ((root (expand-file-name directory)))
    (mapcar #'(lambda(file)
                (list (substring file 0 -4)
                      (expand-file-name file root)))
            (directory-files
             root
             nil
             "[[:lower:]]\\{2\\}_[[:upper:]]\\{2\\}\\.dic"))))

(defun zeppa/fix-find-regexp (list)
  (mapcar #'(lambda(entry)
              (if (string-match "\\^find" (car entry))
                  (list (string-replace "^find" "find\\.exe" (car entry))
                        (cadr entry))
                entry))
          list))

(cond

 ((string= system-type "windows-nt")
  (setq
   ;; make `rgrep' work
   find-program
   (expand-file-name "bin/find.exe"
                     (getenv "ChocolateyInstall")))

  (with-eval-after-load 'grep
    (setq grep-mode-font-lock-keywords
          (zeppa/fix-find-regexp grep-mode-font-lock-keywords)))

  ;; make `hunspell' work
  (let ((root (expand-file-name ".dicts"
                                (or (getenv "HOME")
                                    (getenv "HOMEPATH")))))
    (setenv "DICPATH" root)
    (setenv "DICTIONARY" "en_US")
    (setq ispell-hunspell-dict-paths-alist (zeppa/list-dicts root))))

 ((string= system-type "darwin")
  (menu-bar-mode 1)
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
