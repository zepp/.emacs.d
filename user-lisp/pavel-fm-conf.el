;; file management and shells  -*- lexical-binding: t; -*-

(require 'user-compat)
(require 'user-common)

(use-package dired
  :bind (("C-x C-d" . dired)
         ("C-x M-d" . dired-other-window)
         ("C-x M-j" . dired-jump-other-window)
         (:map dired-mode-map
               ("c")

               ("$" . user/eshell-jump)
               ("M-$" . shell)

               ;; approach that is similar to one that is employed by Magit and
               ;; Org-mode
               ("TAB" . dired-hide-subdir) ;; translated from <tab> and C-i
               ("<backtab>" . dired-hide-all)

               ("Z" . dired-do-compress-to) ;; dired-do-compress
               ("M-*" . dired-mark-files-regexp)))

  :init
  ;; -l is mandatory
  ;; -o is like -l, but do not list group information
  (setopt dired-listing-switches "-aoh")

  :config
  (cond
   ((user/sys-linux-p)
    (add-to-list 'dired-guess-shell-alist-user '("\\.pdf$" "xdg-open"))
    (add-to-list 'dired-guess-shell-alist-user '("\\.odt$" "xdg-open")))))

(use-package dired-aux
  :config
  (add-to-list 'dired-compress-files-alist
               '("\\.7z$" . "7z a %o %i")))

(defun user/command-buf-name (orig &rest args)
  "Changes output buffer name to a command name"

  (let* ((command (nth 0 args))
         (shell-command-buffer-name-async (format "*async: %s*" command)))
    (apply orig args)))

(advice-add 'async-shell-command :around #'user/command-buf-name)

(use-package shell
    :bind (:map shell-command-mode-map
                ;; similar to compilation mode
                ("C-c C-k" . user/kill-process))
    :hook
    (shell-mode-hook . shell-dirtrack-mode)
    (shell-command-mode-hook . read-only-mode))

(use-package eshell
  ;; '$' is a last part of eshell prompt. It is similar to Magit keybinding that
  ;; shows git command history.
  :bind ("C-x $" . user/eshell-jump)
  :hook
  (eshell-directory-change-hook . user/rename-eshell-buf))

(use-package urgrep
  :bind (:map search-map
              ("g" . urgrep))
  :ensure t)

(use-package powershell
  ;; reserved to select other window
  :bind (:map powershell-mode-map ("M-`"))
  :ensure t)

(provide 'pavel-fm-conf)
