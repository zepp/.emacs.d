;;-------------------------------------------------------------------------------
;; global keys definition

(global-set-key (kbd "C-<") #'beginning-of-buffer)
(global-set-key (kbd "C->") #'end-of-buffer)
(global-set-key (kbd "C-#") #'dictem-run-define)
(global-set-key (kbd "C-$") #'ispell-word)
(global-set-key (kbd "C-%") #'query-replace)
(global-set-key (kbd "C-t") 'nil)
(global-set-key (kbd "M-t") #'complete-symbol)

(global-set-key (kbd "C-x p") #'previous-buffer)
(global-set-key (kbd "C-x n") #'next-buffer)
(global-set-key (kbd "C-x d") #'dired-jump)
(global-set-key (kbd "C-x j") #'eshell-jump)
(global-set-key (kbd "C-x C-d") #'dired)
(global-set-key (kbd "C-x M-d") #'dired-other-window)
(global-set-key (kbd "C-x M-f") #'find-file-other-window)
(global-set-key (kbd "C-x M-b") #'switch-to-buffer-other-window)

(global-set-key (kbd "C-c C-g") #'rgrep)
(global-set-key (kbd "C-c C-f") #'find-dired)
(global-set-key (kbd "C-c M-s") #'split-window-vertically)
(global-set-key (kbd "C-c s") #'split-window-horizontally)
(global-set-key (kbd "C-c q") #'delete-other-windows)
(global-set-key (kbd "C-c k") #'delete-window)
(global-set-key (kbd "C-c r") #'iresize-mode)

;;-------------------------------------------------------------------------------
;; related to global key redefinition so placed here

(add-hook 'diff-mode-hook
          '(lambda ()
             ;; diff-goto-source
             (define-key diff-mode-map (kbd "C-m") 'diff-goto-source)))

;;-------------------------------------------------------------------------------
;; global run map

(define-prefix-command 'global-run-map)
(define-key global-run-map (kbd "c") 'calculator)
;; generic mailer
(define-key global-run-map (kbd "m")
  '(lambda ()
     (interactive)
     (if (not (get-buffer wl-folder-buffer-name))
         (wl)
       (switch-to-buffer wl-folder-buffer-name))))
;; rss/atom feed reader 
(define-key global-run-map (kbd "n") 'newsticker-show-news)
;; agenda
(define-key global-run-map (kbd "a") 'org-agenda)
;; jabber
(define-key global-run-map (kbd "j") 
  '(lambda ()
     (interactive)
     (if (get-buffer jabber-roster-buffer)
         (jabber-switch-to-roster-buffer)
       (jabber-connect-with-secrets))))

(define-key global-run-map (kbd "l") 'ielm)

(define-key global-run-map (kbd "f") 'sunrise)

(global-set-key (kbd "C-x M-e") global-run-map)

;;-------------------------------------------------------------------------------
;; platform specific

(when (string= system-type "windows-nt")
  (global-set-key (kbd "<apps>") 'execute-extended-command))
