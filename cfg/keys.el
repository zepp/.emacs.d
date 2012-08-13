;;-------------------------------------------------------------------------------
;; global keys definition

(global-set-key (kbd "C-x M-o") #'(lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-x l") #'next-buffer)
(global-set-key (kbd "M-t") 'dabbrev-completion)
(global-set-key (kbd "C-t") 'nil)
(global-set-key (kbd "M-#") 'dictem-run-define)
(global-set-key (kbd "C-c C-r") 'rgrep)
(global-set-key (kbd "C-x M-f") 'find-file-other-window)
(global-set-key (kbd "C-x ^") 'iresize-mode)

;;-------------------------------------------------------------------------------
;; related to global key redefinition so placed here

(add-hook 'diff-mode-hook
          '(lambda ()
             ;; diff-goto-source
             (define-key diff-mode-map (kbd "C-m") 'diff-goto-source)))

;;-------------------------------------------------------------------------------

(add-hook 'sh-mode-hook
	  #'(lambda ()
	      (define-key sh-mode-map (kbd "C-c C-r") nil)))

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
