;; my configuration of various programming modes -*- lexical-binding: t; -*-

;;-------------------------------------------------------------------------------

(require 'user-common)
(require 'pavel-text-conf)

(add-to-list 'display-buffer-alist
             '((derived-mode . prog-mode)
               (display-buffer-reuse-window
                display-buffer-reuse-mode-window
                display-buffer-use-some-window
                display-buffer-pop-up-window)
               (mode . prog-mode)
               (inhibit-switch-frame . t)))

(use-package super-save
  :config
  (setopt auto-save-default nil
          super-save-auto-save-when-idle t
          super-save-silent t)
  (add-to-list 'super-save-predicates #'u-prog-mode-p)
  (super-save-mode 1)
  :ensure t
  :demand t)

(defun u-prog-preserve-case ()
  (setq-local case-fold-search nil
              case-replace t))

(use-package prog-mode
  :bind (:map prog-mode-map
              ("C-c C-q" . fill-paragraph))
  :hook
  (prog-mode-hook . u-prog-preserve-case)
  (prog-mode-hook . visual-line-mode)
  (prog-mode-hook . whitespace-mode)
  (prog-mode-hook . auto-revert-mode))

(use-package elisp-mode
  :bind ([remap fill-paragraph] . lisp-fill-paragraph)
  :hook (emacs-lisp-mode-hook . eldoc-mode)
  :bind (:map emacs-lisp-mode-map
              ("C-c C-d" . debug-on-entry)))

(use-package web-mode
  :mode ("\\.html\\'" . web-mode)
  :ensure t)

(use-package typescript-mode
  :ensure t)

(use-package corfu
  :hook (prog-mode-hook . corfu-mode)
  :ensure t)

(use-package rg
  :ensure t)

(defun u-projectile-eshell ()
  "Starts `eshell' in a project's root directory."
  (interactive)

  (u-eshell-jump (projectile-project-root)))

(use-package projectile
  :bind-keymap ("C-x p" . projectile-command-map)
  :bind (:map projectile-command-map
              ("$" . u-projectile-eshell)
              ("g" . projectile-search)
              ("F" . projectile-find-file-dwim))

  :init
  (projectile-mode 1)
  (recentf-mode 1)
  (editorconfig-mode 1)

  :config
  (setopt
   projectile-indexing-method 'hybrid
   projectile-git-use-fd t
   projectile-mode-line-prefix " "
   projectile-completion-system 'default
   projectile-search-backend 'ripgrep)

  (push ".angular" projectile-globally-ignored-directories)
  (push ".husky" projectile-globally-ignored-directories)
  (push ".nx" projectile-globally-ignored-directories)

  :ensure t)

;;-------------------------------------------------------------------------------
;; version control

(use-package diff-mode
  :bind (:map diff-mode-map
              ("C-M-n" . diff-file-next)
              ("C-M-p" . diff-file-prev)
              ("C-j" . diff-goto-source)))

(use-package ediff
  :init
  (setopt
   ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package smerge-mode
  :bind (:map smerge-mode-map
              ("C-c C-p" . smerge-prev)
              ("C-c C-n" . smerge-next)
              ("C-c C-c" . smerge-keep-current)
              ("C-c C-r" . smerge-resolve)
              ("C-c C-e" . smerge-ediff)
              ("C-c *" . smerge-refine)
              ("C-c +" . smerge-combine-with-next)))

(use-package magit
  :after ido
  :bind (("C-x C-v" . magit-file-dispatch) ;; find-alternate-file
         (:map magit-section-mode-map
               ("M-1")
               ("M-2")
               ("M-3")
               ("C-<tab>")
               ;; familiar keybindings
               ("C-M-u" . magit-section-up)
               ("C-M-f" . magit-section-forward-sibling)
               ("C-M-b" . magit-section-backward-sibling)))

  :config
  (setopt
   magit-define-global-key-bindings nil
   magit-display-buffer-function
   'magit-display-buffer-fullframe-status-v1)

  ;; append to end of hook list
  (add-hook 'git-commit-setup-hook 'turn-off-auto-fill t)
  (add-hook 'git-commit-setup-hook #'text/ws-long-lines)
  (add-to-list 'ido-ignore-buffers "^magit-process: .*")
  (add-to-list 'ido-ignore-buffers ".*\\.~[[:alnum:]]+~$")
  ;; to avoid a garbage in a process buffer
  (push "FORCE_COLOR=0" magit-git-environment)

  (add-to-list 'display-buffer-alist
               '((derived-mode . magit-log-mode)
                 (display-buffer-reuse-mode-window
                  display-buffer-at-bottom)
                 (window-height . 0.25)
                 (inhibit-switch-frame . t)))

  :ensure t)

(use-package ssh-agency
  :demand t
  :ensure t)

;;-------------------------------------------------------------------------------
;; Docker, CI/CD and etc

(use-package yaml-mode
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(provide 'pavel-prog-conf)
