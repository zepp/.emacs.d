(setq helm-gtags-prefix-key (kbd "M-j")
      helm-gtags-suggested-key-mapping nil
      helm-gtags-use-input-at-cursor t
      helm-gtags-ignore-case t
      helm-gtags-auto-update t
      helm-gtags-update-interval-second nil
      helm-gtags-fuzzy-match t)

;; ggtags style shortcuts
(let ((map (define-prefix-command 'my-helm-gtags-prefix-mode-map)))
  (define-key map (kbd "M-p") #'helm-gtags-previous-history)
  (define-key map (kbd "M-n") #'helm-gtags-next-history)
  (define-key map (kbd "M-f") #'helm-gtags-find-files)
  (define-key map (kbd "M-g") #'helm-gtags-find-pattern)
  (define-key map (kbd "M-o") #'helm-gtags-find-symbol)
  (define-key map (kbd "M-r") #'helm-gtags-find-rtag)
  (define-key map (kbd "M-t") #'helm-gtags-dwim)
  (define-key helm-gtags-mode-map helm-gtags-prefix-key map))
(define-key helm-gtags-mode-map [remap xref-pop-marker-stack] #'helm-gtags-pop-stack)

(add-hook 'c-mode-common-hook #'helm-gtags-mode)
