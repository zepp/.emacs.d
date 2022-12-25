(require 'org-agenda)

(setq
 calendar-week-start-day 1
 org-catch-invisible-edits 'show-and-error
 org-time-stamp-custom-formats '("<%a %d %b %Y>" . "<%a %H:%M %d %b %Y>")
 org-agenda-window-setup 'current-window)

(add-hook 'org-mode-hook #'org-indent-mode)
(add-hook 'org-mode-hook #'org-toggle-time-stamp-overlays)
(add-hook 'org-mode-hook #'flyspell-mode)

(add-to-list 'auto-mode-alist '("\\.org\\(-mode\\)?$" . org-mode))

(defadvice org-agenda-redo-all
    (around reload-files (command &optional exhaustive)
            activate)
  "it reloads files from disk before rebuilding agenda"
  (dolist (file org-agenda-files)
    (find-file-noselect file)))
