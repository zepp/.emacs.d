(require 'jabber)

(jabber-activity-mode 1)
(jabber-mode-line-mode -1)

(setq
 jabber-activity-banned '("juick@juick\.com" "jubo@nologin\.ru" "psto@psto\.net")
 jabber-auto-reconnect t
 jabber-chat-delayed-time-format "%H:%M"
 jabber-default-status nil
 jabber-use-global-history nil
 jabber-history-dir (expand-file-name "jabber" my-emacs-var-dir)
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

(add-hook 'jabber-chat-mode-hook
          '(lambda()
             (flyspell-mode t)
             (auto-fill-mode t)))

(add-hook 'jabber-post-connect-hooks 'jabber-autoaway-start)
