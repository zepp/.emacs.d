(setq
 elscreen-tab-display-control nil
 elscreen-tab-display-kill-screen nil
 elscreen-mode-to-nickname-alist
 '(("^dired-mode$" .
    (lambda ()
      (format "%s" (buffer-name))))
   ("^Info-mode$" .
    (lambda ()
      (format "Info(%s)" (file-name-nondirectory Info-current-file))))
   ("^irchat-" . "IRChat")))

(define-key elscreen-map (kbd "l") 'elscreen-toggle)
(define-key elscreen-map (kbd "a") 'elscreen-screen-nickname)
(elscreen-start)

