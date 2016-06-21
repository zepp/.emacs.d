(require 'org-mobile)

(setq
 calendar-week-start-day 1
 org-clock-persist t
 org-mobile-force-mobile-change t
 org-mobile-directory "~/Dropbox/Apps/MobileOrg"
 org-agenda-window-setup 'current-window)

(define-key org-mode-map (kbd "C-c C-x C-a") nil)
(define-key org-agenda-mode-map (kbd "C-c C-x C-a") nil)

(org-clock-persistence-insinuate)

(add-hook 'org-mode-hook #'org-indent-mode)
