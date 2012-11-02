;; depends on cc-mode because c-mode-map defined there
(require 'cc-mode)

(setq gtags-suggested-key-mapping nil
      gtags-auto-update t)

(require 'gtags)

(define-key gtags-mode-map (kbd "C-c v") #'gtags-visit-rootdir)
(define-key gtags-mode-map (kbd "M-,") #'gtags-find-tag-from-here)
(define-key gtags-mode-map (kbd "M-.") #'gtags-find-tag)
(define-key gtags-mode-map (kbd "C-M-,") #'gtags-find-rtag)
(define-key gtags-mode-map (kbd "M-^") #'gtags-pop-stack)

(add-hook 'c-mode-common-hook #'gtags-mode)
