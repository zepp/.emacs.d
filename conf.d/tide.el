(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

;; aligns annotation to the right hand side
(setq tide-completion-setup-company-backend t
      company-tooltip-align-annotations t
      tide-completion-detailed t)

(defun tide-unversal-rename ()
  "wrapper around `tide-rename-file' and
`tide-rename-symbol'. Specify any universal argument to rename
file"
  (interactive)

  (if current-prefix-arg
      (tide-rename-file)
    (tide-rename-symbol)))

(define-key tide-mode-map (kbd "C-c C-j") #'tide-jump-to-definition)
(define-key tide-mode-map (kbd "C-c C-u") #'tide-jump-back)
(define-key tide-mode-map (kbd "C-c C-f") #'tide-references)
(define-key tide-mode-map (kbd "C-c C-n") #'tide-unversal-rename)
(define-key tide-mode-map (kbd "C-c C-d") #'tide-documentation-at-point)
(define-key tide-mode-map (kbd "C-c C-l") #'tide-project-errors)
(define-key tide-mode-map (kbd "C-c C-c") #'compile)
(define-key tide-mode-map (kbd "C-M-\\") #'tide-format)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)
(add-hook 'js2-mode-hook #'setup-tide-mode)
