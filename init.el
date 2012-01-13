;;(setq max-lisp-eval-depth (* 1024 1024))

(setq inhibit-startup-screen t)
(transient-mark-mode 1)
(menu-bar-mode 0)
(tool-bar-mode 0)
(toggle-scroll-bar -1)
(column-number-mode 1)

(setq x-select-enable-clipboard t)

(put 'scroll-left 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(require 'warnings)
(add-to-list 'warning-suppress-types '(undo discard-info))

(add-to-list 'load-path "~/.emacs.d/loadable/")

(setq my-emacs-var-dir "~/.emacs.var"
      my-emacs-personal-cfg (expand-file-name "personal" user-emacs-directory))

(defun load-ext (cfg &rest name-dirs)

  (defun do-load (cfg)
    (load (format (expand-file-name "cfg/%s.el" user-emacs-directory) cfg))
    (let ((path (expand-file-name (concat cfg ".el") my-emacs-personal-cfg)))
      (if (file-regular-p path)
          (load path)
        nil)))

  (let ((name (first name-dirs))
        (dirs (rest name-dirs)))
    (if name
        (let ((load-root (expand-file-name name "~/elisp")))
          (if (find-if #'(lambda (path)
                           (search name path))
                       load-path)
              (do-load cfg)
            (let ((path-list
                   (if dirs
                       (mapcar #'(lambda (dir)
                                   (expand-file-name dir load-root))
                               dirs)
                     (list load-root))))
              (when (not (remove t
                                 (mapcar 
                                  #'(lambda (path)
                                      (file-directory-p path))
                                  path-list)))
                (setq load-path (nconc path-list load-path))
                (do-load cfg)))))
      (do-load cfg))))

(setq auto-save-list-file-prefix (expand-file-name "auto-save/" my-emacs-var-dir))

;;-------------------------------------------------------------------------------

;; iswitchb
(require 'iswitchb)
(setq iswitchb-regexp t)
(iswitchb-mode 1)

;;-------------------------------------------------------------------------------

(require 'whitespace)
(global-whitespace-mode 0)
(setq
 whitespace-style '(indentation space-before-tab
                                space-after-tab))

;;-------------------------------------------------------------------------------

;; save openned file
(require 'desktop)
(desktop-save-mode 1)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(setq 
 ;; emacs is launched in daemon mode from .profile so I have to place
 ;; this directive here to avoid hanging at window manager boot-up
 desktop-load-locked-desktop t
 desktop-files-not-to-save
 "\\(^/[^/:]*:\\|(ftp)$\\)\\|\\(.*\\.gpg$\\)\\|\\(.*\\.el\\.gz$\\)\\|\\(.*\\.[chSs]$\\)\\|\\(.*\\.el$\\)"
 desktop-path `(,my-emacs-var-dir))

;;-------------------------------------------------------------------------------

(require 'calculator)
(setq calculator-electric-mode nil)

;;-------------------------------------------------------------------------------

(require 'compile)
(setq compilation-error-regexp-metaware
      '("[Ew] \"\\(.*\\)\",L\\([0-9]+\\)/C\\([0-9]+\\)\\((#[0-9]+)\\)?:\t\
\\(.*\\)"
        1 2 3))
(setq compilation-error-regexp-alist
      `(,compilation-error-regexp-metaware gnu gcc-include))

;;-------------------------------------------------------------------------------

(require 'epa)
(setenv "GPG_AGENT_INFO" nil)

;;-------------------------------------------------------------------------------

(require 'ediff)

(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function #'(lambda (&optional arg)
                                      (if (> (frame-width) 140)
                                          (split-window-horizontally arg)
                                        (split-window-vertically arg))))

;;-------------------------------------------------------------------------------

(require 'lisp-mode)
(define-key lisp-mode-shared-map (kbd "M-t") 'lisp-complete-symbol)

;;-------------------------------------------------------------------------------

(require 'cperl-mode)

(fset 'perl-mode 'cperl-mode)

(add-hook 'cperl-mode-hook
          (lambda ()
            (cperl-set-style "C++")
            (define-key cperl-mode-map (kbd "C-h f") 'cperl-perldoc)))

;;-------------------------------------------------------------------------------

(require 'cl)
(setq auto-mode-alist (acons ".*\\.bb$" 'shell-script-mode auto-mode-alist))
(setq auto-mode-alist (acons "Makefile\\..*" 'makefile-mode auto-mode-alist))
(setq auto-mode-alist (acons ".*\\.h\\.in$" 'c-mode auto-mode-alist))

;;-------------------------------------------------------------------------------

;; external packages or big configuration statements should be moved
;; to separate file
(load-ext "cc")
(load-ext "fonts")
(load-ext "env")
(load-ext "org" "org-7.4" "lisp")
(load-ext "gtags" "global")
(load-ext "prefs" "prefs")
(load-ext "zencolor" "zenburn-el")
(load-ext "fs")

(if (string= system-type "windows-nt")
    (load-ext "dsvn" "dsvn")
  (load-ext "dictem" "dictem")
  (load-ext "nt" "newsticker-1.99")
  (load-ext "jabber" "emacs-jabber")
  (load-ext "wl" "wanderlust-2.15.9" "wl" "elmo" "utils")
  (load-ext "magit" "magit")
  (load-ext "w3m" "w3m")
  (load-ext "erc" "erc")
  (load-ext "gdb")
  (load-ext "mpc"))

;;-------------------------------------------------------------------------------

(load "my-utils")
(load "buffer-recode")
(ring-insert evm-coding-systems-list 'windows-1251)
(ring-insert evm-coding-systems-list 'koi8-r)
(global-set-key [f5] 'recode-buffer)

;;-------------------------------------------------------------------------------
;; loaded finally to be sure that all mode maps are available

(load-ext "keys")

;;-------------------------------------------------------------------------------

(add-to-list 'default-frame-alist '(font . "terminus-14"))

(when (not (string= system-type "windows-nt"))
  (when (functionp 'newsticker-start-ticker)
    (newsticker-start-ticker))
  (when (functionp 'wl)
    (wl 1)))
