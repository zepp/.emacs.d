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
 use-package-hook-name-suffix nil
 calendar-week-start-day 1)

(add-to-list 'warning-suppress-types '(undo discard-info))

(add-to-list 'same-window-buffer-names "*grep*")

(add-hook 'kill-emacs-hook #'basic-save-buffer)

(setf frame-title-format "%b")

(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))

;;-------------------------------------------------------------------------------
;; simple.el

(setq visual-line-fringe-indicators '(nil right-curly-arrow))

;; disable tab indentation globally
(setq-default indent-tabs-mode nil)

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

(use-package browse-url
  :init
  (setq
   browse-url-browser-function 'browse-url-chrome
   browse-url-chrome-program "brave")
  :defer t)

;;-------------------------------------------------------------------------------

(use-package uniquify
  :init (setq uniquify-buffer-name-style 'forward
              uniquify-ignore-buffers-re "^\\*"))

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

;;-------------------------------------------------------------------------------
;; UI related

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

(defun zeppa/insert-double-q-marks ()
  "it inserts double angle quotation marks"
  (interactive)

  (save-excursion (insert "«»"))
  (forward-char 1))

(use-package text-mode
  :bind (:map text-mode-map
              ("C-q" . #'zeppa/insert-double-q-marks))
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
   org-tags-column 0
   org-log-into-drawer t
   org-catch-invisible-edits 'show-and-error
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

;; general commands start from C-c
(global-set-key (kbd "C-c g") #'rgrep)
(global-set-key (kbd "C-c w") #'browse-url)
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
             "[[:lower:]]\\{2\\}_[[:upper:]]\\{2\\}\\.aff"))))

(cond

 ((string= system-type "windows-nt")
  (setq
   ;; make `rgrep' work
   find-program
   (expand-file-name "bin/find.exe"
                     (getenv "ChocolateyInstall"))

   browse-url-chrome-program
   (expand-file-name
    "brave.exe"
    "C:\\Program Files\\BraveSoftware\\Brave-Browser\\Application"))

  ;; make `hunspell' work
  (let ((root (expand-file-name ".dicts"
                                (or (getenv "HOME")
                                    (getenv "HOMEPATH")))))
    (setenv "DICPATH" root)
    (setenv "DICTIONARY" "en_US")
    (setq ispell-hunspell-dict-paths-alist (zeppa/list-dicts root))))

 ((string= system-type "darwin")
  (setq mac-command-modifier 'meta
        mac-option-modifier 'super)))

;;-------------------------------------------------------------------------------

(load custom-file :noerror)
