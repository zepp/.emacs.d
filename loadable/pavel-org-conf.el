;; my Org-mode configuration and helper functions -*- lexical-binding: t; -*-

(require 'pavel-text-conf)

(use-package org-agenda
  :bind (("C-x C-a" . org-agenda)
         (:map org-agenda-mode-map
               ("C-x C-n" . org-agenda-capture)
               ("C-c C-w") ;; org-agenda-refile

               ("n" . org-agenda-next-item)
               ("p" . org-agenda-previous-item)
               ("C-M-n" . org-agenda-next-date-line)
               ("C-M-p" . org-agenda-previous-date-line)

               ("k" . org-agenda-kill)
               ("d" . org-agenda-deadline)
               ("s" . org-agenda-schedule)
               ("r" . org-agenda-refile)
               ("C-c C-p" . org-agenda-set-property)))
  :init
  (setq
   org-agenda-window-setup 'current-window
   org-agenda-start-on-weekday nil
   org-agenda-start-day "-1d"
   org-agenda-remove-tags t
   org-agenda-use-time-grid nil
   org-agenda-bulk-custom-functions
   '((?a org-agenda-archive-default))))

(use-package org-capture
  :bind (("C-x C-n" . org-capture)
         (:map org-capture-mode-map
               ("C-c C-r" . org-capture-refile)))
  :init
  (add-to-list 'display-buffer-alist
               '("CAPTURE-\\w+\\.org$"
                 (display-buffer-pop-up-window
                  display-buffer-use-some-window)
                 (dedicated . t)
                 (window-min-width . 60))))

(defun pavel/org-current-timestamp ()
  "Inserts an inactive timestamp without prompt."
  (interactive)
  (org-timestamp '(16) 'inactive))

(use-package org
  :bind (("C-x &" . org-store-link)
         (:map org-mode-map
               ("C-x C-n" . org-insert-heading-respect-content)
               ("C-c &") ;; org-mark-ring-goto
               ("C-c M-." . pavel/org-current-timestamp)
               ("M-m" . org-emphasize)
               ("C-M-f" . org-forward-element)
               ("C-M-b" . org-backward-element)
               ("C-M-u" . org-up-element)
               ("C-M-d" . org-down-element)
               ("C-M-n" . org-next-visible-heading)
               ("C-M-p" . org-previous-visible-heading)
               ("C-M-SPC" . org-mark-element)
               ("C-c C-w" . org-cut-special)
               ("C-c M-w" . org-copy-special)
               ("C-c C-y" . org-paste-special)
               ("C-c C-r" . org-refile)
               ("C-c M-r" . org-refile-copy)
               ("C-c C-p" . org-set-property)
               ("C-c C-b" . org-mark-ring-goto)
               ("C-c C-x C-r" . org-reveal)))

  :init
  (setq
   org-hide-emphasis-markers t
   org-tags-column 0
   org-log-into-drawer t
   org-log-done 'time
   org-fold-catch-invisible-edits 'show-and-error
   org-time-stamp-custom-formats '("<%a %d %b %Y>" . "<%a %H:%M %d %b %Y>")
   org-goto-auto-isearch nil
   org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
   org-id-uuid-program
   (cond ((string= system-type "gnu/linux")
          "uuidgen | awk '{print toupper($0)}'")
         ((string= system-type "windows-nt") "guidgen /u")
         (t "uuidgen"))

   org-export-with-date nil
   org-export-with-tags nil
   org-export-with-toc nil
   org-export-with-section-numbers nil
   org-export-initial-scope 'subtree)

  (add-to-list 'display-buffer-alist
               '((or
                  ;; there is a whitespace at the beginning of the buffer name
                  "\\*Agenda Commands\\*$"
                  "\\*Org Export Dispatcher\\*$"
                  "\\*Org Select\\*$"
                  "\\*Org Attach\\*$"
                  "\\*Org Links\\*$"
                  "\\*Org Help\\*$"
                  "\\*Select Link\\*$"
                  (major-mode . calendar-mode))
                 display-buffer-at-bottom
                 (dedicated . t)))

  (add-to-list
   'display-buffer-alist
   ;; attachment directory naming is an UUID but first part is shorten
   '("^[0-9a-f]\\{6\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}$"
     display-buffer-below-selected
     (window-height . 0.25)))

  (add-to-list 'display-buffer-alist
               '((or
                  "\\*Org Table Edit Field\\*$"
                  "\\*Edit Formulas\\*$")
                 display-buffer-below-selected
                 (dedicated . t)
                 (window-height . 5)))

  :config
  (add-to-list 'org-latex-packages-alist '("AUTO" "babel" t ("pdflatex")))
  (add-to-list 'org-latex-packages-alist '("" "tabularx"))
  (add-to-list 'org-latex-packages-alist '("" "spverbatim"))

  :hook
  (org-mode-hook . org-indent-mode)
  (org-mode-hook . org-toggle-time-stamp-overlays)
  (org-mode-hook . auto-revert-mode)
  (org-mode-hook . flyspell-mode)

  :mode
  (("\\.org\\'" . org-mode)))

(use-package ox-html
  :after org
  :init
  (setq
   ;; highlight.js to be used
   org-html-htmlize-output-type nil
   org-html-self-link-headlines t
   org-html-head-include-default-style nil
   org-html-head-include-scripts nil
   org-html-postamble t)

  :config
  (add-to-list 'org-html-postamble-format
               `("ru"
                 ,(concat
                   "<p class=\"author\">%a</p>"
                   "<p class=\"timestamp\">%T</p>")))

  (add-to-list 'org-html-special-string-regexps
               '("—" . "&#x2014;"))
  (add-to-list 'org-html-special-string-regexps
               '("–" . "&#x2013;")))

;;;###autoload
(defun pavel/set-org-dir-files (directory)
  "sets Org-mode's notes directory, default and archive files
location"
  (interactive "D")

  (setq
   org-directory directory
   org-agenda-files `(,directory)
   org-default-notes-file (expand-file-name "default.org" directory)
   org-archive-location (expand-file-name "archive.org::" directory)))

;;;###autoload
(defun pavel/org-agenda-upcase-uuids ()
  "upcases UUIDs in all Org-mode agenda files"
  (interactive)

  (let ((agenda-buffers (org-buffer-list 'agenda))
        (counter 0))
    (dolist (buffer agenda-buffers)
      (with-current-buffer buffer
        (if (buffer-modified-p)
            (message "%s is modified, so skipping it" (current-buffer))
          (pavel/upcase-buffer-uuids)
          (when (buffer-modified-p)
            (basic-save-buffer)
            (setq counter (1+ counter))))))
    (when (> counter 0)
      (message "%i/%i buffers are updated"
               counter (length agenda-buffers)))))

;; oherwise `org-open-at-point' fails when a link points to an attachment
(require 'org-attach)

(provide 'pavel-org-conf)
