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

(define-key tide-mode-map (kbd "C-c C-j") #'tide-jump-to-definition)
(define-key tide-mode-map (kbd "C-c C-f") #'tide-references)
(define-key tide-mode-map (kbd "C-c C-n") #'tide-rename-symbol)
(define-key tide-mode-map (kbd "C-M-\\") #'tide-format)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)
(add-hook 'js2-mode-hook #'setup-tide-mode)
