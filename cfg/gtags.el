;; depends on cc-mode because c-mode-map defined there
(require 'cc-mode)

(setq gtags-suggested-key-mapping nil
      gtags-auto-update t)

(require 'gtags)

(define-key gtags-mode-map (kbd "C-c v") #'gtags-visit-rootdir)
(define-key gtags-mode-map (kbd "M-r") #'gtags-find-rtag)
(define-key gtags-mode-map (kbd "M-s") #'gtags-find-symbol)
(define-key gtags-mode-map (kbd "M-^") #'gtags-pop-stack)
(define-key gtags-mode-map (kbd "M-j") #'gtags-find-tag-from-here)
(define-key gtags-mode-map (kbd "M-J") #'gtags-find-tag)
(define-key gtags-select-mode-map (kbd "M-^") #'gtags-pop-stack)
(define-key gtags-select-mode-map (kbd "j") #'gtags-select-tag)
(define-key gtags-select-mode-map (kbd "M-j") #'gtags-select-tag-other-window)

(add-hook 'c-mode-common-hook #'gtags-mode)
