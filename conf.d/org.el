(require 'org-agenda)

(setq
 calendar-week-start-day 1
 org-tags-column 0
 org-catch-invisible-edits 'show-and-error
 org-time-stamp-custom-formats '("<%a %d %b %Y>" . "<%a %H:%M %d %b %Y>")
 org-agenda-remove-tags t
 org-agenda-window-setup 'current-window)

(add-hook 'org-mode-hook #'org-indent-mode)
(add-hook 'org-mode-hook #'org-toggle-time-stamp-overlays)
(add-hook 'org-mode-hook #'auto-revert-mode)
(add-hook 'org-mode-hook #'flyspell-mode)

(add-to-list 'auto-mode-alist '("\\.org\\(-mode\\)?$" . org-mode))

(setq
 org-export-with-toc nil
 org-export-with-section-numbers nil
 org-export-initial-scope 'subtree
 org-html-postamble nil)
