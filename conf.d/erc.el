(require 'erc)
(setq
 erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE" "328")
 erc-track-exclude-server-buffer t
 erc-track-shorten-start 5
 erc-track-shorten-cutoff 9
 erc-auto-query 'bury)

(erc-autojoin-mode t)
(erc-track-mode t)

(require 'erc-nicklist)
(setq erc-nicklist-use-icons nil)

(require 'erc-log)
(setq 
 erc-log-channels-directory (expand-file-name "erc" local-conf-dir)
 erc-save-buffer-on-part t
 erc-truncate-buffer-on-save t)
(erc-log-enable)

(require 'erc-autoaway)
(setq
 erc-autoaway-idle-seconds 600
 erc-auto-discard-away t)

(require 'erc-truncate)
(erc-truncate-mode t)
(setq erc-max-buffer-size 10240)

(defun bitlbee ()
  "Starts the ERC connection to the local bitlbee server"

  (interactive)
  (unless (get-buffer "&bitlbee")
    (require 'secrets)
    (erc :server "localhost"
         :nick bitlbee-nick 
         :password bitlbee-password)))
