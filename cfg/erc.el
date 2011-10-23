(require 'erc)
(setq
 erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE" "328"))

(erc-autojoin-mode t)
(erc-track-mode t)

(require 'erc-nicklist)
(setq erc-nicklist-use-icons nil)

(require 'erc-log)
(setq 
 erc-log-channels-directory (expand-file-name "erc" my-emacs-var-dir)
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
