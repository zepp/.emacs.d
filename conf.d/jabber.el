(jabber-activity-mode 1)
(jabber-mode-line-mode -1)

(setq
 jabber-auto-reconnect t
 jabber-chat-delayed-time-format "%H:%M"
 jabber-default-status nil
 jabber-use-global-history nil
 jabber-history-enable-rotation t
 jabber-history-enabled t
 jabber-history-size-limit 128
 jabber-rare-time-format "%a %d %b %Y"
 jabber-roster-line-format   " %u %n %S"
 jabber-resource-line-format "     | %r:%p %S"
 jabber-roster-show-bindings nil
 jabber-roster-show-title nil
 jabber-show-offline-contacts nil
 jabber-vcard-avatars-publish nil
 jabber-vcard-avatars-retrieve nil
 ;; jabber.el hangs after it receives 'composing' event from gajim if
 ;; this variable is t
 jabber-chatstates-confirm nil
 ;; jabber-message-echo is removed from both hooks to avoid noisy
 ;; messages in minibuffer
 jabber-alert-presence-hooks '()
 jabber-alert-message-hooks '(jabber-message-scroll)
 jabber-autoaway-method 'jabber-xprintidle-get-idle-time)

(mapcar
 #'(lambda (h)
     (add-hook 'jabber-chat-mode-hook h))
 `(flyspell-mode longlines-mode ,#'(lambda () (bury-buffer "*-jabber-roster-*"))))

(add-hook 'jabber-post-connect-hooks 'jabber-autoaway-start)

(add-hook 'jabber-roster-mode-hook
          #'(lambda ()
              (define-key jabber-roster-mode-map (kbd "C-c C-l") 
                #'jabber-activity-switch-to)
              (define-key jabber-roster-mode-map (kbd "M-n") 
                #'jabber-go-to-next-jid)
              (define-key jabber-roster-mode-map (kbd "M-p") 
                #'jabber-go-to-previous-jid)
              (define-key jabber-roster-mode-map (kbd "j") 
                #'jabber-roster-ret-action-at-point)
              (define-key jabber-roster-mode-map (kbd "M-j") 
                #'jabber-compose)
              (define-key jabber-roster-mode-map (kbd "C-j") 
                #'jabber-groupchat-join)))

(add-hook 'jabber-chat-mode-hook
          #'(lambda ()
              (define-key jabber-chat-mode-map (kbd "M-RET")
                #'newline)
              (define-key jabber-chat-mode-map (kbd "C-c C-l") 
                #'jabber-activity-switch-to)))

(defadvice jabber-chat-with
  (around split-fashion (jc jid &optional other-window) 
          activate)
  "Controls the fashion of window splitting. Splits window
vertically."
  (let ((split-height-threshold 0)
        (split-width-threshold nil))
    ad-do-it))

(when (require 'split-root nil t)

(defun jabber-activity-popup ()
  (interactive)

  (when jabber-activity-jids
    (select-window (split-root-window 10))
    (jabber-activity-switch-to)))

(global-set-key (kbd "C-x C-j C-u") #'jabber-activity-popup))

