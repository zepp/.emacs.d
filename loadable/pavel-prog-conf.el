;; my configuration of various programming modes -*- lexical-binding: t; -*-

;;-------------------------------------------------------------------------------

(use-package prog-mode
  :bind (:map prog-mode-map
              ("C-c C-q" . fill-paragraph))
  :hook
  (prog-mode-hook . visual-line-mode)
  (prog-mode-hook . whitespace-mode)
  (prog-mode-hook . auto-revert-mode))

(use-package elisp-mode
  :bind ([remap fill-paragraph] . lisp-fill-paragraph)
  :hook (emacs-lisp-mode-hook . eldoc-mode)
  :bind (:map emacs-lisp-mode-map
              ("C-c C-d" . debug-on-entry)))

(use-package company
  :after prog-mode
  :bind ((:map company-mode-map
               ("C-M-i" . company-complete))
         (:map company-active-map
               ("M-i" . company-complete-selection)))
  :hook (prog-mode-hook . company-mode)
  :ensure t)

(use-package tree-sitter-langs
  :ensure t
  :demand t)

;;;###autoload
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

(use-package json-ts-mode
  :after prog-mode
  :bind (:map json-ts-mode-map
              ("C-c C-p" . json-pretty-print)))

;; required by projectile
(use-package ag
  ;; used by `search-scope-compose-ag-args'
  :autoload (ag/search)

  :init
  (setq ag-reuse-buffers t)
  (search-scope-register-symbolic-engine
   #'ag/search
   #'search-scope-compose-ag-args)

  :ensure t)

;;;###autoload
(defun pavel/projectile-commander-other-tab ()
  "Starts projectile commander and overrides a default action to
display a buffer."
  (interactive)

  ;; `projectile-completion-system' is overridden since default completion
  ;; utilizes a bottom window.
  (let ((projectile-completion-system 'ido)
        (display-buffer-overriding-action '(display-buffer-in-new-tab)))
    (projectile-commander)))

(defun pavel/projectile-eshell ()
  "Starts `eshell' in a project's root directory."
  (interactive)

  (pavel/eshell-jump (projectile-project-root)))

(use-package projectile
  :bind-keymap ("C-x p" . projectile-command-map)
  :bind ((:map tab-prefix-map
               ("p" . pavel/projectile-commander-other-tab))
         (:map projectile-command-map
               ("$" . pavel/projectile-eshell)
               ("g" . projectile-ag)
               ("F" . projectile-find-file-dwim)))
  :init
  (setf
   projectile-mode-line-prefix " "
   projectile-completion-system 'default)
  (projectile-mode 1)
  :config
  (push "node_modules" projectile-globally-ignored-directories)
  (push ".angular" projectile-globally-ignored-directories)
  (push ".husky" projectile-globally-ignored-directories)
  (push ".nx" projectile-globally-ignored-directories)
  (push #'search-scope-index-projectile-files search-scope-indexers)
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
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally))

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

  :init
  (setq
   magit-define-global-key-bindings nil
   magit-display-buffer-function
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

(provide 'pavel-prog-conf)
