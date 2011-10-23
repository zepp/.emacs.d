;;-------------------------------------------------------------------------------
;; global keys definition

;; (global-set-key (kbd "M-o") 'other-window)
;; (global-set-key (kbd "M-0") 'delete-window)
;; (global-set-key (kbd "M-1") 'delete-other-windows)
;; (global-set-key (kbd "M-2") 'split-window-vertically)
;; (global-set-key (kbd "M-3") 'split-window-horizontally)
(global-set-key (kbd "M-C") 'previous-buffer)
(global-set-key (kbd "M-c") 'next-buffer)
(global-set-key (kbd "M-t") 'dabbrev-completion)
(global-set-key (kbd "M-#") 'dictem-run-define)

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
;; web browser
(define-key global-run-map (kbd "w") 'w3m)
;; rss/atom feed reader 
(define-key global-run-map (kbd "n") 'newsticker-show-news)
;; agenda
(define-key global-run-map (kbd "a") 'org-agenda)
;; slime
(define-key global-run-map (kbd "s") 'slime)
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

(define-key global-run-map (kbd "i") 
  '(lambda ()
     "Connect to ERC, or switch to last active buffer"
     (interactive)
     (if (get-buffer (format "%s:%i" erc-server erc-port))
         (erc-track-switch-buffer 1) ;; yes: switch to last active
       (progn
         (require 'secrets)
         (erc-tls :server erc-server :port erc-port :nick erc-nick
                  :password erc-password :full-name erc-user-full-name)))))

(define-key global-run-map (kbd "l") 'ielm)

(define-key global-run-map (kbd "p") 'mpc)

(global-set-key (kbd "C-x M-e") global-run-map)

;;-------------------------------------------------------------------------------
;; platform specific

(when (string= system-type "windows-nt")
  (global-set-key (kbd "<apps>") 'execute-extended-command))
