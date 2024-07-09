(require 'cl)
(require 'warnings)

(menu-bar-mode 0)
(tool-bar-mode 0)
(toggle-scroll-bar -1)
(column-number-mode 1)

(transient-mark-mode 1)
(delete-selection-mode 1)

(electric-pair-mode 1)

(setq
 inhibit-startup-screen t
 visible-bell t
 make-pointer-invisible t
 x-select-enable-clipboard t
 default-input-method 'russian-computer
 custom-file "~/.emacs.d/custom.el"
 use-package-hook-name-suffix nil)

(put 'scroll-left 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(add-to-list 'warning-suppress-types '(undo discard-info))

(add-to-list 'same-window-buffer-names "*grep*")

(add-hook 'kill-emacs-hook #'basic-save-buffer)

(setenv "GPG_AGENT_INFO" nil)

(defvar ignored-buffer-list
  '("\\*Completions" "\\*Quail Completions\\*" "\\*Backtrace\\*" "\\*magit-edit-log\\*" "\\*P4" "\\*Buffer List\\*")
  "list of the buffer names or regular expressions to be ignored by
various buffer management routines")

;;-------------------------------------------------------------------------------

(defun suitable-buffer-p (buffer)
  "predicate to check the buffer exclusion from the `ignored-buffer-list'"
  (not (find-if #'(lambda (entry)
                    (string-match entry (buffer-name buffer)))
                ignored-buffer-list)))

(setf frame-title-format "%F")
(add-to-list 'default-frame-alist `(buffer-predicate . ,#'suitable-buffer-p))
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))

;;-------------------------------------------------------------------------------

(add-to-list 'auto-mode-alist '("Makefile\\..*" . makefile-mode))
(add-to-list 'auto-mode-alist '(".*\\.mak$" . makefile-mode))
(add-to-list 'auto-mode-alist '(".*\\.h\\.in$" . c-mode))

;;-------------------------------------------------------------------------------
;; simple.el

(global-visual-line-mode 1)
(setq visual-line-fringe-indicators '(nil right-curly-arrow))

(defadvice async-shell-command
    (around split-fashion (command &optional output-buffer error-buffer)
            activate)
  "Changes output buffer name to a command name"
  (let ((shell-command-buffer-name-async (format "*async: %s*" command)))
        ad-do-it))

;;-------------------------------------------------------------------------------
;; package management

(let ((default-directory "~/.emacs.d/loadable/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(require 'my-utils)

(require 'package)
(add-to-list
 'package-archives
 '("melpa" . "http://stable.melpa.org/packages/")
 t)
(package-initialize)

;;-------------------------------------------------------------------------------

(use-package browse-url
  :init
  (setq
   browse-url-browser-function 'browse-url-chrome
   browse-url-chrome-program "brave")
  :defer t)

;;-------------------------------------------------------------------------------

(use-package text-mode
  :hook (text-mode-hook . flyspell-mode)
  :defer t)

;;-------------------------------------------------------------------------------

(defun zeppa/elisp-mode()
  "it disables tabs indentation and enables documentation hints in
mini-buffer"
  (setq-local indent-tabs-mode nil)
  (eldoc-mode 1))

(use-package elisp-mode
  :hook (emacs-lisp-mode-hook . zeppa/elisp-mode)
  :defer t)

;;-------------------------------------------------------------------------------

(use-package whitespace
  :init (setq whitespace-style '(face
                                 trailing
                                 space-after-tab
                                 space-before-tab))
  :config (global-whitespace-mode 1))

;;-------------------------------------------------------------------------------
;; diff

(use-package diff-mode
  :bind (:map diff-mode-map
              ("C-m" . diff-goto-source))
  :defer t)

;;-------------------------------------------------------------------------------

(use-package uniquify
  :init (setq uniquify-buffer-name-style 'forward
              uniquify-ignore-buffers-re "^\\*"))

;;-------------------------------------------------------------------------------
;; ;; https://200ok.ch/posts/2020-08-22_setting_up_spell_checking_with_multiple_dictionaries.html

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

;;-------------------------------------------------------------------------------

(defadvice dired-do-shell-command
    (around split-fashion (command &optional arg file-list)
            activate)
  "Controls the fashion of window splitting. Splits window
vertically."
  (let ((split-height-threshold 0)
        (split-width-threshold nil))
    ad-do-it))

(use-package dired
  :bind (:map dired-mode-map
              (("M-n" . dired-next-line)
               ("M-p" . dired-previous-line)
               ("c" . dired-do-copy)
               ("d" . dired-do-delete)
               ("r" . dired-do-rename)
               ("M-d" . dired-flag-file-deletion)))

  :init (setq dired-recursive-deletes 'always
              dired-deletion-confirmer #'y-or-n-p
              ;; -l is mandatory
              ;; -G omit the group name
              ;; -h human-readable size
              dired-listing-switches "-alGh")
  :defer t)

;;-------------------------------------------------------------------------------

(use-package expand-region
  :bind ("M-@" . er/expand-region)
  :ensure t)

(use-package which-key
  :init (setq which-key-idle-delay 2.0)
  :config (which-key-mode 1)
  :ensure t)

(use-package company
  :bind ("M-t" . company-complete)
  :config (global-company-mode 1)
  :ensure t)

;;-------------------------------------------------------------------------------

(use-package ivy
  :bind (("C-c C-s" . swiper)
         ("C-c C-r" . ivy-resume)

         :map ivy-minibuffer-map
         ("M-t" . ivy-partial-or-done))

  :init (setq ivy-use-virtual-buffers t
              ivy-count-format "[%d/%d] ")

  :config (dolist (buf ignored-buffer-list)
            (add-to-list 'ivy-ignore-buffers buf))

  ;; doesn't work if it's placed in :config
  :hook (after-init-hook . ivy-mode)

  :ensure t)

(use-package ido
  :init (setq ido-enable-flex-matching t
              ido-create-new-buffer 'always)
  :config (dolist (buf ignored-buffer-list)
            (add-to-list 'ido-ignore-buffers buf))
  :disabled t)

;;-------------------------------------------------------------------------------

(use-package powerline
  :config (powerline-default-theme)
  :ensure t)

;;-------------------------------------------------------------------------------

(use-package tree-sitter-langs
  :after tree-sitter
  :ensure t)

(use-package flycheck
  :ensure t)

(use-package compile
  :init
  (setq
   compilation-auto-jump-to-first-error 'if-location-known
   compilation-scroll-output 'first-error)

  :config
  (let ((regexp
         ;; groups: 1 - file, 2 - line, 3 - column
         '(webpack "ERROR in \\([^(\r\n]+\\)\(\\([0-9]+\\),\\([0-9]+\\)\)?$" 1 2 3)))
    (add-to-list 'compilation-error-regexp-alist-alist regexp)
    (add-to-list 'compilation-error-regexp-alist (car regexp)))

  (defadvice compile
      (around split-fashion (command &optional comint)
              activate)
    "Controls the fashion of window splitting. Splits window vertically."
    (let ((split-height-threshold 0)
          (split-width-threshold nil))
      ad-do-it))

  :defer t)

(use-package css-mode
  :bind
  (:map css-mode-map
        ("C-c C-c" . compile))
  :defer t)

(use-package json-mode
  :bind
  (:map json-mode-map
        ("C-c C-c" . compile))
  :defer t
  :ensure t)

(defun zeppa/tide-mode ()
  "setup tide mode in current buffer"
  (interactive)

  (tide-setup)
  (tide-hl-identifier-mode 1)
  (flycheck-mode 1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode 1))

(defun zeppa/tide-rename ()
  "wrapper around `tide-rename-file' and
`tide-rename-symbol'. Specify a universal argument to rename
file"
  (interactive)

  (if current-prefix-arg
      (tide-rename-file)
    (tide-rename-symbol)))

(use-package tide
  :bind
  (:map tide-mode-map
        ("C-c C-j" . tide-jump-to-definition)
        ("C-c C-u" . tide-jump-back)
        ("C-c C-f" . tide-references)
        ("C-c C-n" . zeppa/tide-rename)
        ("C-c C-d" . tide-documentation-at-point)
        ("C-c C-l" . tide-project-errors)
        ("C-c C-c" . compile)
        ("C-M-\\" . tide-format))

  :init
  (setq tide-completion-setup-company-backend t
        company-tooltip-align-annotations t
        tide-completion-detailed t)

  :hook
  (typescript-mode-hook . zeppa/tide-mode)
  (typescript-ts-mode-hook . zeppa/tide-mode)
  (tsx-ts-mode-hook . zeppa/tide-mode)
  (js2-mode-hook . zeppa/tide-mode)
  (before-save-hook . tide-format-before-save)

  :after (flycheck)

  :defer t
  :ensure t)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;;(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
;;(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

;;-------------------------------------------------------------------------------

(use-package org
  :bind ("C-x C-a" . org-agenda)

  :init
  (setq
   calendar-week-start-day 1
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-time-stamp-custom-formats '("<%a %d %b %Y>" . "<%a %H:%M %d %b %Y>")
   org-agenda-remove-tags t
   org-agenda-window-setup 'current-window

   org-export-with-toc nil
   org-export-with-section-numbers nil
   org-export-initial-scope 'subtree
   org-latex-tables-centered nil
   ;; https://orgmode.org/manual/CSS-support.html
   org-html-self-link-headlines t
   org-html-head-include-default-style nil
   org-html-head-include-scripts nil
   org-html-head "<script src=\"dist/bundle.js\"></script>"
   org-html-head-extra "<meta property=\"og:type\" content=\"article\" />"
   org-html-postamble nil
   org-ascii-verbatim-format "\"%s\""
   org-ascii-text-width 1000)

  :config
  (add-to-list 'org-latex-packages-alist '("AUTO" "babel" t ("pdflatex")))
  (add-to-list 'org-latex-packages-alist '("" "tabularx"))

  :hook
  (org-mode-hook . org-indent-mode)
  (org-mode-hook . org-toggle-time-stamp-overlays)
  (org-mode-hook . auto-revert-mode)
  (org-mode-hook . flyspell-mode)

  :defer t)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;;-------------------------------------------------------------------------------

(global-set-key (kbd "C-x p") #'previous-buffer)
(global-set-key (kbd "C-x c") #'shell-jump)
(global-set-key (kbd "C-x M-f") #'find-file-at-point)
(global-set-key (kbd "C-x C-x") #'server-edit)

;; general commands start from C-c
(global-set-key (kbd "C-c g") #'rgrep)
(global-set-key (kbd "C-c w") #'browse-url)
;; window management in StumpWM style :)
(global-set-key (kbd "C-c s") #'split-window-horizontally)
(global-set-key (kbd "C-c v") #'split-window-vertically)
(global-set-key (kbd "C-c q") #'delete-other-windows)
(global-set-key (kbd "C-c k") #'delete-window)
(global-set-key (kbd "C-c o") #'other-window)

(load custom-file :noerror)
