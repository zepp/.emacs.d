;;-------------------------------------------------------------------------------

(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org\\(-mode\\)?$" . org-mode))

(setq org-agenda-files '("~/dropbox/org/agenda.org.gpg")
      org-agenda-window-setup 'current-window)

(add-hook 'org-mode-hook #'(lambda () 
			     ;; C-t is reserved by the stumpWM so this
			     ;; binding will be very handy
			     (define-key org-mode-map (kbd "C-c t") #'org-todo)
			     (define-key org-agenda-mode-map (kbd "C-c t") #'org-agenda-todo)))

;;-------------------------------------------------------------------------------
