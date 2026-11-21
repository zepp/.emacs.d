;; my configuration of text modes -*- lexical-binding: t; -*-

(require 'user-compat)
(require 'user-common)
(require 'typography-mode)

(use-package whitespace
  :init
  (setopt
   whitespace-style '(face
                      trailing
                      space-after-tab
                      space-before-tab)))

(defun text/ws-long-lines ()
  "highlights long lines using `whitespace-mode'"

  (setq-local whitespace-style (cons 'lines-char whitespace-style)))

(use-package visual-fill-column
  :init
  (setq-default fill-column 80)

  :ensure t)

(defun u-hunpsell-dicts(directory)
  "builds dictionary path alist for hunspell"

  (let ((root (expand-file-name directory)))
    (mapcar #'(lambda(file)
                (list (substring file 0 -4)
                      (expand-file-name file root)))
            (directory-files
             root
             nil
             "[[:lower:]]\\{2\\}_[[:upper:]]\\{2\\}\\.dic"))))

(use-package ispell
  :init
  (when (u-sys-win-p)
    ;; make `hunspell' work
    (let ((root (expand-file-name ".dicts"
                                  (or (getenv "HOME")
                                      (getenv "HOMEPATH")))))
      (setenv "DICPATH" root)
      (setenv "DICTIONARY" "en_US")
      (setopt ispell-hunspell-dict-paths-alist (u-hunpsell-dicts root))))
  (setopt
   ispell-program-name "hunspell"
   ispell-dictionary "ru_RU,en_US"
   ispell-personal-dictionary "~/.hunspell_personal")

  :config
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "ru_RU,en_US")
  (u-touch-file ispell-personal-dictionary))

(use-package text-mode
  :bind (:map text-mode-map
              ("C-c C-q" . fill-paragraph)
              ("C-c C-o" . browse-url))
  :hook
  (text-mode-hook . visual-line-mode)
  (text-mode-hook . whitespace-mode)
  (text-mode-hook . u-minor-text-modes))

(defun u-minor-text-modes ()
  "checks current buffer major mode then enables additional minor modes
usefull for text editing"

  (abbrev-mode 1)
  (visual-line-fill-column-mode 1)

  ;; pseudo text modes are excluded
  (unless (derived-mode-p '(html-mode nxml-mode))
    (flyspell-mode 1)
    (typography-mode 1))

  ;; to avoid a confilict with `org-indent-mode'
  (unless (eq major-mode 'org-mode)
    (visual-wrap-prefix-mode 1)))

(use-package markdown-mode
  :bind (:map markdown-mode-map
              ;; marker keybindings resembling Org-mode
              ("M-m *" . markdown-insert-bold)
              ("M-m /" . markdown-insert-italic)
              ("M-m `" . markdown-insert-code)
              ("M-m =" . markdown-insert-code)
              ;; movement
              ("C-M-u" . markdown-outline-up)
              ("C-M-n" . markdown-outline-next)
              ("C-M-p" . markdown-outline-previous)
              ("C-M-f" . markdown-outline-next-same-level)
              ("C-M-b" . markdown-outline-previous-same-level)
              ("C-M-k" . markdown-kill-block)
              ("C-M-SPC" . markdown-mark-block))
  :ensure t)

(provide 'pavel-text-conf)
