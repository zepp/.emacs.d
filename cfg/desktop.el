;; save openned file
(require 'desktop)
(desktop-save-mode 1)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'sr-mode)
(setq 
 ;; emacs is launched in daemon mode from .profile so I have to place
 ;; this directive here to avoid hanging at window manager boot-up
 desktop-load-locked-desktop t
 desktop-files-not-to-save
 "\\(^/[^/:]*:\\|(ftp)$\\)\\|\\(.*\\.gpg$\\)\\|\\(.*\\.el\\.gz$\\)\\|\\(.*\\.[chSs]$\\)\\|\\(.*\\.el$\\)"
 desktop-path `(,my-emacs-var-dir))


