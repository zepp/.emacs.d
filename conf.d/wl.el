;; wanderlust


(autoload 'wl "wl" "wanderlust" t)
(autoload 'wl-draft "wl" "Write draft with Wanderlust." t)
(autoload 'wl-user-agent-compose "wl-draft" nil t)

;; ssl.el
(setq
 ssl-certificate-verification-policy 1
 ssl-program-name "gnutls-cli"
 ssl-program-arguments '("-p" service host))

(setq
 ;;disable internal flim decoder
 mel-b-ccl-module nil

 elmo-imap4-default-stream-type 'ssl
 elmo-imap4-default-port 993
 elmo-imap4-default-authenticate-type 'clear
 elmo-imap4-debug t

 elmo-pop3-default-stream-type 'ssl
 elmo-pop3-default-authenticate-type 'user
 elmo-pop3-debug t

 wl-folder-check-async 1
 wl-folder-use-frame nil
 wl-folder-buffer-name "wl-folders"

 wl-message-ignored-field-list '("^.*")
 wl-message-visible-field-list '("^From:" "^To:" "^Cc:" "^Date:" "^Subject:" "^User-Agent:" "^X-Mailer:" "^Content-Type:")
 wl-message-sort-field-list    wl-message-visible-field-list
 ;; to avoid noise message
 wl-message-id-domain "fc5697d5365100a6a82bc87acbda"
 wl-message-id-use-wl-from 1

 ;; for autorefile
 wl-draft-reply-buffer-style 'full
 wl-draft-config-matchone t
 wl-draft-preview-attributes-buffer-lines 7
 wl-draft-always-delete-myself t
 ;; Only save draft when I tell it to! (C-x C-s or C-c C-s):
 wl-auto-save-drafts-interval nil

 signature-file-prefix (expand-file-name "~/.emacs.d/personal/wl/")
 signature-delete-blank-lines-at-eof t
 signature-insert-at-eof t
 signature-separator ""

 wl-summary-auto-refile-skip-marks nil
 wl-summary-line-format "%W %D %M %h:%m %T%P %4S %t%[%c %f% %] %s"
 wl-summary-width nil

 ;; mark sent messages (folder carbon copy) as read.
 wl-fcc-force-as-read t

 wl-generate-mailer-string-function 'wl-generate-user-agent-string-1)

(add-hook 'wl-mail-setup-hook 'wl-draft-config-exec)

(add-hook 'wl-mail-setup-hook
          '(lambda()
             (save-excursion
               (end-of-buffer)
               (wl-draft-insert-signature))) t)

;; semi + flim
(setq
 mime-transfer-level 8
 mime-edit-split-message nil
 mime-edit-message-max-length 32768
 ;; process quoted headers (email from, to and etc.)
 mime-header-accept-quoted-encoded-words t)

(setq wl-folders-file (expand-file-name "wl/folders" local-conf-dir)
      wl-address-file (expand-file-name "wl/addresses" local-conf-dir))

(add-hook 'wl-folder-mode-hook
          #'(lambda ()
              (define-key wl-folder-mode-map (kbd "M-n") #'wl-folder-next-unread)
              (define-key wl-folder-mode-map (kbd "M-p") #'wl-folder-prev-unread)
              (define-key wl-folder-mode-map (kbd "M-g") #'wl-folder-goto-folder-sticky)
              (define-key wl-folder-mode-map (kbd "M-j") #'wl-folder-jump-folder)))

(add-hook 'wl-summary-mode-hook
          #'(lambda ()
              ;; useful jumps
              (define-key wl-summary-mode-map (kbd "M-n") #'wl-summary-down)
              (define-key wl-summary-mode-map (kbd "M-p") #'wl-summary-up)
              ;; I don't like the shortcuts starting from capital letter
              (define-key wl-summary-mode-map (kbd "M-g") #'wl-folder-goto-folder-sticky)
              (define-key wl-summary-mode-map (kbd "M-a") #'wl-summary-reply-with-citation)
              (define-key wl-summary-mode-map (kbd "M-d") #'wl-summary-delete)
              (define-key wl-summary-mode-map (kbd "M-u") #'wl-summary-unmark-all)
              ;; it's handy then press '/'
              (define-key wl-summary-mode-map (kbd "\\") #'wl-thread-open-close)
              ;; frequently used functions should be accessible 
              (define-key wl-summary-mode-map (kbd "s") #'wl-summary-sync-update)
              (define-key wl-summary-mode-map (kbd "M-s") #'wl-summary-pick)

              ;; let's make the complex shortcuts a little bit easy to
              ;; complete
              (define-key wl-summary-mode-map (kbd "m r")
                #'wl-summary-target-mark-mark-as-read)
              (define-key wl-summary-mode-map (kbd "m m")
                #'wl-summary-target-mark-all)
              (define-key wl-summary-mode-map (kbd "m a")
                #'wl-summary-target-mark-reply-with-citation)
              (define-key wl-summary-mode-map (kbd "m M-d")
                #'wl-summary-target-mark-delete)
              ;; region commands
              (define-key wl-summary-mode-map (kbd "r r") 
                #'wl-summary-mark-as-read-region)
              (define-key wl-summary-mode-map (kbd "r m") 
                #'wl-summary-target-mark-region)
              (define-key wl-summary-mode-map (kbd "r M-d")
                #'wl-summary-delete-region)
              ;; thread command
              (define-key wl-summary-mode-map (kbd "t r")
                #'wl-thread-mark-as-read)
              (define-key wl-summary-mode-map (kbd "t M-d")
                #'wl-thread-delete)))

(add-hook 'wl-summary-mode-hook #'wl-summary-toggle-disp-msg)

(add-hook 'wl-draft-mode-hook
          #'(lambda ()
              (define-key wl-draft-mode-map (kbd "C-c C-b") #'mail-text)))

(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'wl-user-agent))
(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'wl-user-agent
      'wl-user-agent-compose
      'wl-draft-send
      'wl-draft-kill
      'mail-send-hook))

(defun wl-summary-zepp-get-short-name (str)
  (let ((pet-name (elmo-get-hash-val
                   (downcase
                    (wl-address-header-extract-address str)) 
                   wl-address-petname-hash)))
    (cond
     ((and pet-name (> (length pet-name) 0)) pet-name)
     ((string-match "\\(.*[^ \t]\\)[ \t]*<[^>]*>" str)
      (wl-match-string 1 str))
     (t str))))
