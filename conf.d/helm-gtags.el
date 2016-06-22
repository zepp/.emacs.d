(setq helm-gtags-prefix-key (kbd "C-t")
      helm-gtags-suggested-key-mapping nil
      helm-gtags-use-input-at-cursor t
      helm-gtags-ignore-case t
      helm-gtags-auto-update t
      helm-gtags-update-interval-second nil
      helm-gtags-fuzzy-match t)

;; this keymap is mostly duplicate default one but handles C-t problem
(define-key helm-gtags-mode-map (kbd "C-t h") #'helm-gtags-display-browser)
(define-key helm-gtags-mode-map (kbd "C-t P") #'helm-gtags-find-files)
(define-key helm-gtags-mode-map (kbd "C-t f") #'helm-gtags-parse-file)
(define-key helm-gtags-mode-map (kbd "C-t g") #'helm-gtags-find-pattern)
(define-key helm-gtags-mode-map (kbd "C-t s") #'helm-gtags-find-symbol)
(define-key helm-gtags-mode-map (kbd "C-t r") #'helm-gtags-find-rtag)
(define-key helm-gtags-mode-map (kbd "C-t t") #'helm-gtags-find-tag)
(define-key helm-gtags-mode-map (kbd "C-t d") #'helm-gtags-find-tag)
(define-key helm-gtags-mode-map (kbd "C-t C-t") #'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "M-*") #'helm-gtags-pop-stack)

(add-hook 'c-mode-common-hook #'helm-gtags-mode)
