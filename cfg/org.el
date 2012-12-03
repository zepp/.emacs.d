;;-------------------------------------------------------------------------------

(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org\\(-mode\\)?$" . org-mode))

(setq
 org-mobile-directory "~/dropbox/mobile-org"
 org-agenda-window-setup 'current-window)

(add-hook 'org-mode-hook
	  #'(lambda () 
	      ;; C-t is reserved by the stumpWM so this
	      ;; binding will be very handy
	      (define-key org-mode-map (kbd "C-c t") #'org-todo)
	      (define-key org-agenda-mode-map (kbd "C-c t") #'org-agenda-todo)
	      ;; mobile-org shortcuts
	      (define-key org-agenda-mode-map (kbd "C-c p") #'org-mobile-push)
	      (define-key org-agenda-mode-map (kbd "C-c M-p") #'org-mobile-pull)))

;;-------------------------------------------------------------------------------
