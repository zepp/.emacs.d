;; custom-set-faces was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.

(if (string= system-type "windows-nt")
    (custom-set-faces
     '(default ((t (:height 110 :family "Lucida Console")))))
  (custom-set-faces
   '(default ((t (:height 110 :family "Dejavu Sans Mono"))))
   '(jabber-activity-face ((t (:weight bold))))
   '(jabber-chat-error ((t (:foreground "dark red" :weight bold))))
   '(jabber-chat-prompt-foreign ((t (:foreground "grey"))))
   '(jabber-chat-prompt-local ((t (:foreground "white"))))
   '(jabber-chat-prompt-system ((t (:foreground "dark green" :weight bold))))
   '(jabber-chat-text-foreign ((t (:foreground "grey"))))
   '(jabber-roster-user-away ((t (:foreground "grey" :slant italic))))
   '(jabber-roster-user-dnd ((t (:foreground "orange4" :slant italic))))
   '(jabber-roster-user-online ((t (:slant normal))))
   '(jabber-roster-user-xa ((t (:foreground "grey" :slant italic))))
   '(jabber-title-medium ((t (:family "dejavu sans mono" :weight bold :foreground "orange3"))))
   '(jabber-title-small ((t (:family "dejavu sans mono" :weight bold))))))

(add-to-list 'default-frame-alist '(font . "dejavu sans mono-11"))
