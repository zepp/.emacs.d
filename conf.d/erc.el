(setq
 ;; 305 - "Welcome back"
 ;; 306 - "You're now away"
 erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE" "305" "306")
 erc-track-exclude-server-buffer t
 erc-track-shorten-start 5
 erc-track-shorten-cutoff 9
 erc-auto-query 'bury)

(erc-autojoin-mode t)
(erc-track-mode t)

(setq 
 erc-log-channels-directory (expand-file-name "erc" local-conf-dir)
 erc-save-buffer-on-part t
 erc-truncate-buffer-on-save t)
(erc-log-mode 1)

(setq
 erc-auto-set-away t
 erc-autoaway-idle-seconds 600
 erc-auto-discard-away t)
(erc-autoaway-mode 1)

(setq erc-max-buffer-size 10240)
(erc-truncate-mode 1)

(defun bitlbee-auto-login ()
  "login on all registered accounts"
  (when (boundp 'bitlbee-acc-num)
    (when (and (string= erc-default-server erc-session-server)
               (string= "&bitlbee" (buffer-name)))
      (dotimes (i bitlbee-acc-num)
        (erc-message "PRIVMSG"
                     (format "%s acc %d on"
                             (erc-default-target) i))))))

(add-hook 'erc-join-hook 'bitlbee-auto-login)
