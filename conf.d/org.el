(setq
 org-mobile-force-mobile-change t
 org-mobile-directory "~/Dropbox/mobile-org"
 org-id-locations-file (expand-file-name "org/.org-id-locations" local-conf-dir)
 org-agenda-window-setup 'current-window)

(add-hook 'org-mode-hook
          #'(lambda () 
              ;; C-t is reserved by the stumpWM so this
              ;; binding will be very handy
              (define-key org-mode-map (kbd "C-c t") #'org-todo)
              (define-key org-agenda-mode-map (kbd "C-c t") #'org-agenda-todo)
              (define-key org-agenda-mode-map (kbd "M-n") #'org-agenda-next-item)
              (define-key org-agenda-mode-map (kbd "M-p") #'org-agenda-previous-item)
              (define-key org-agenda-mode-map (kbd "C-M-n") #'org-agenda-next-date-line)
              (define-key org-agenda-mode-map (kbd "C-M-p") #'org-agenda-previous-date-line)
              ;; mobile-org shortcuts
              (define-key org-agenda-mode-map (kbd "C-c C-p") #'org-mobile-push)
              (define-key org-agenda-mode-map (kbd "C-c C-u") #'org-mobile-pull)))

