;; file management and shells  -*- lexical-binding: t; -*-

(use-package dired
  :bind (("C-x C-d" . dired)
         ("C-x M-d" . dired-other-window)
         ("C-x M-j" . dired-jump-other-window)
         (:map dired-mode-map
               ("c")

               ("$" . pavel/eshell-jump)
               ("M-$" . shell)

               ;; approach that is similar to one that is employed by Magit and
               ;; Org-mode
               ("TAB" . dired-hide-subdir) ;; translated from <tab> and C-i
               ("<backtab>" . dired-hide-all)

               ("Z" . dired-do-compress-to) ;; dired-do-compress
               ("M-*" . dired-mark-files-regexp)))

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

(defun pavel/command-buf-name-advice (orig &rest args)
  "Changes output buffer name to a command name"

  (let* ((command (nth 0 args))
         (shell-command-buffer-name-async (format "*async: %s*" command)))
    (apply orig args)))

(advice-add 'async-shell-command :around #'pavel/command-buf-name-advice)

(if (pavel/emacs-29-p)
    (use-package shell
      :bind (:map shell-mode-map
                ;; similar to compilation mode
                ("C-c C-k" . pavel/kill-process))
      :hook
      (shell-mode-hook . shell-dirtrack-mode))
  (use-package shell
    :bind (:map shell-command-mode-map
                ;; similar to compilation mode
                ("C-c C-k" . pavel/kill-process))
    :hook
    (shell-mode-hook . shell-dirtrack-mode)
    (shell-command-mode-hook . read-only-mode)))

(defun pavel/eshell-buf-name (&optional directory)
  "it provides eshell buffer name that includes directory
name. Format is simillar to the `project-eshell'"

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

(defun pavel/rename-eshell-buf()
  "hook that keeps eshell buffer name actual"

  (let* ((name (pavel/eshell-buf-name))
         (buf (get-buffer name)))
    (if buf
        ;; do not cause entry to the debugger
        (user-error "failed to actualize Emacs shell buffer name")
      (rename-buffer name))))

(use-package eshell
  ;; '$' is a last part of eshell prompt. It is similar to Magit keybinding that
  ;; shows git command history.
  :bind ("C-x $" . pavel/eshell-jump)
  :init
  ;; this special variable is not marked as auto-loadable, so to avoid condition
  ;; when one is already defined in a lexical scope I have to make this hack to
  ;; make `pavel/eshell-jump' work.
  (defvar eshell-buffer-name "*eshell*")
  :hook
  (eshell-directory-change-hook . pavel/rename-eshell-buf))

(use-package grep
  :config
  (setq grep-find-ignored-directories
        (append grep-find-ignored-directories
                '("node_modules"
                  ".angular"
                  ".nx"
                  ".vscode"))
        grep-find-ignored-files
        (append grep-find-ignored-files
                '("chunk-*.js*")))

  (when (string= system-type "windows-nt")
    (setq grep-mode-font-lock-keywords
          (pavel/fix-find-regexp grep-mode-font-lock-keywords))))

(provide 'pavel-fm-conf)
