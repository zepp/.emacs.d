;; my configuration of text modes -*- lexical-binding: t; -*-

(use-package whitespace
  :init
  (setq whitespace-style '(face
                           trailing
                           space-after-tab
                           space-before-tab)))

;;;###autoload
(defun pavel/ws-long-lines ()
  "highlights long lines using `whitespace-mode'"

  (setq-local whitespace-style (cons 'lines-char whitespace-style)))

(use-package visual-fill-column
  :init
  (setq-default fill-column 80)

  :ensure t)

(use-package ispell
  :init
  (setq ispell-program-name "hunspell"
        ispell-dictionary "ru_RU,en_US"
        ispell-personal-dictionary "~/.hunspell_personal")

  :config
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "ru_RU,en_US")
  (pavel/touch-file ispell-personal-dictionary))

(defun pavel/touch-file (path)
  (unless (file-exists-p path)
    ;; If START is a string, then output that string to the file instead of any
    ;; buffer contents. END is ignored.
    (write-region "" nil path nil 0)))

(use-package text-mode
  :bind (:map text-mode-map
              ("C-c C-q" . fill-paragraph)
              ("C-c C-o" . browse-url))
  :hook
  (text-mode-hook . visual-line-mode)
  (text-mode-hook . whitespace-mode)
  (text-mode-hook . pavel/minor-text-modes))

(defun pavel/minor-text-modes ()
  "checks current buffer major mode then enables additional minor modes
usefull for text editing"

  ;; pseudo-text modes are excluded
  (unless (derived-mode-p 'html-mode 'nxml-mode)
    (flyspell-mode 1)
    (abbrev-mode 1)
    (typography-mode 1)
    (visual-line-fill-column-mode 1)))

(use-package markdown-mode
  :bind (:map markdown-mode-map
              ;; marker keybindings replicating Org-mode
              ("M-m *" . markdown-insert-bold)
              ("M-m /" . markdown-insert-italic)
              ("M-m =" . markdown-insert-code)
              ;; movement
              ("C-M-u" . markdown-outline-up)
              ("C-M-n" . markdown-outline-next)
              ("C-M-p" . markdown-outline-previous)
              ("C-M-f" . markdown-outline-next-same-level)
              ("C-M-b" . markdown-outline-previous-same-level)
              ("C-M-k" . markdown-kill-block)
              ("C-M-SPC" . markdown-mark-block)))

(provide 'pavel-text-conf)
