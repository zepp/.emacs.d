(setq gtags-suggested-key-mapping nil
      gtags-auto-update t)

(define-key gtags-mode-map (kbd "C-c C-v") #'gtags-visit-rootdir)
(define-key gtags-mode-map (kbd "C-c C-r") #'gtags-find-rtag)
;; isearch word & highlighting, I don't use both
(define-key gtags-mode-map (kbd "M-s") #'gtags-find-symbol)
(define-key gtags-mode-map (kbd "M-^") #'gtags-pop-stack)
;; indent new comment, ahh this one was useful
(define-key gtags-mode-map (kbd "M-j") #'gtags-find-tag-from-here)
(define-key gtags-mode-map (kbd "C-M-j") #'gtags-find-tag)
(define-key gtags-select-mode-map (kbd "M-^") #'gtags-pop-stack)
(define-key gtags-select-mode-map (kbd "j") #'gtags-select-tag)
(define-key gtags-select-mode-map (kbd "M-j") #'gtags-select-tag-other-window)

(add-hook 'c-mode-common-hook #'gtags-mode)
