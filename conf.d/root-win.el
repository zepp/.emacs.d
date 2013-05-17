(defun jabber-activity-new-window ()
  (interactive)

  (when jabber-activity-jids
    (select-window (split-root-window 10))
    (jabber-activity-switch-to)))

(global-set-key (kbd "C-x C-j C-u") #'jabber-activity-new-window)
