(setq
 ;; 305 - "Welcome back"
 ;; 306 - "You're now away"
 erc-track-position-in-mode-line t
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

