;;-------------------------------------------------------------------------------
;; global keys definition

(global-set-key (kbd "C-x M-o") #'(lambda () (interactive) (other-window -1)))
(global-set-key (kbd "M-C") 'previous-buffer)
(global-set-key (kbd "M-c") 'next-buffer)
(global-set-key (kbd "M-t") 'dabbrev-completion)
(global-set-key (kbd "C-t") 'nil)
(global-set-key (kbd "M-#") 'dictem-run-define)
(global-set-key (kbd "C-c C-r") 'rgrep)
(global-set-key (kbd "C-x M-f") 'find-file-other-window)

;;-------------------------------------------------------------------------------
;; related to global key redefinition so placed here

;; (add-hook 'wl-summary-mode-hook
;;           '(lambda()
;;              ;; wl-summary-refile-prev-destination
;;              (define-key wl-summary-mode-map (kbd "M-o") 'nil)))

(add-hook 'wl-folder-mode-hook
          '(lambda()
             ;; wl-fldmgr-copy
             (define-key wl-folder-mode-map (kbd "M-c") 'nil)))

(add-hook 'diff-mode-hook
          '(lambda ()
             ;; diff-goto-source
             ;;(define-key diff-mode-map (kbd "M-o") 'nil)
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
       (progn
         (require 'secrets)
         (jabber-connect-all)
         ;;(jabber-switch-to-roster-buffer) does not work
         (switch-to-buffer jabber-roster-buffer)))))

(define-key global-run-map (kbd "l") 'ielm)

(global-set-key (kbd "C-x M-e") global-run-map)

;;-------------------------------------------------------------------------------
;; platform specific

(when (string= system-type "windows-nt")
  (global-set-key (kbd "<apps>") 'execute-extended-command))
