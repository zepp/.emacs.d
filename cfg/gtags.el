;; depends on cc-mode because c-mode-map defined there
(require 'cc-mode)

(require 'gtags)

(define-key c-mode-base-map (kbd "M-,")   'gtags-find-tag-from-here)
(define-key c-mode-base-map (kbd "C-M-,") 'gtags-find-rtag)
(define-key c-mode-base-map (kbd "M-/")   'gtags-find-pattern)
