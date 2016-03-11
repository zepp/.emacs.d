(jabber-activity-mode 1)
(jabber-mode-line-mode -1)

(setf
 jabber-auto-reconnect t
 jabber-history-enabled nil
 jabber-history-muc-enabled nil
 jabber-roster-line-format   " %u %n %S"
 jabber-resource-line-format "     | %r:%p %S"
 jabber-roster-show-bindings nil
 jabber-roster-show-title nil
 ;; to debug the jabber server fancies
 jabber-debug-log-xml nil
 ;; jabber-message-echo is removed from both hooks to avoid noisy
 ;; messages in minibuffer
 jabber-alert-presence-hooks '()
 jabber-alert-message-hooks '(jabber-message-scroll))

(add-hook 'jabber-chat-mode-hook #'flyspell-mode)
;; to avoid emacs daemon hanging on exit
(add-hook 'kill-emacs-hook #'jabber-disconnect)

(define-key jabber-roster-mode-map (kbd "C-c C-l") #'jabber-activity-switch-to)
(define-key jabber-roster-mode-map (kbd "M-n")     #'jabber-go-to-next-roster-item)
(define-key jabber-roster-mode-map (kbd "M-p")     #'jabber-go-to-previous-roster-item)

(define-key jabber-chat-mode-map (kbd "C-RET")   #'newline)
(define-key jabber-chat-mode-map (kbd "C-c C-l") #'jabber-activity-switch-to)

(defadvice jabber-connect-all
    (before  connect-with-secrets (&optional arg)
             activate)
  (require 'secrets))

(require 'point nil t)
(setf point-icon-mode nil)
