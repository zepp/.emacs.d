(require 'cl)
(require 'warnings)

(defvar local-conf-dir
  (expand-file-name "local.d" user-emacs-directory)
  "directory to keep host specific configuration files those
should not be pushed to git repo")

;; old emacs versions 
(unless (boundp 'user-emacs-directory)
  (setq user-emacs-directory "~/.emacs.d/"))

(transient-mark-mode 1)
(menu-bar-mode 0)
(tool-bar-mode 0)
(toggle-scroll-bar -1)
(column-number-mode 1)

(setq
 visible-bell t
 inhibit-startup-screen t
 x-select-enable-clipboard t
 frame-title-format "%F"
 default-input-method 'russian-computer
 lpr-command "xpp"
 auto-save-list-file-prefix
 (expand-file-name "auto-save/" local-conf-dir))

(put 'scroll-left 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))

(add-to-list 'warning-suppress-types '(undo discard-info))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/loadable/"))

;;-------------------------------------------------------------------------------

(defun load-conf (conf &optional sym-or-path)

  (defun do-load (conf)
    (load (expand-file-name (format "conf.d/%s.el" conf)
                            user-emacs-directory))
    (load (expand-file-name (format "%s.el" conf)
                            local-conf-dir) t))

  (if sym-or-path
      (if (symbolp sym-or-path)
          (when (fboundp sym-or-path)
            (do-load conf))
        (let ((rexp (concat "/" (regexp-quote sym-or-path))))
          (when (find-if #'(lambda (path)
                             (string-match rexp path))
                         load-path)
            (do-load conf))))
    
    (do-load conf)))

;;-------------------------------------------------------------------------------

(defvar ignored-buffer-list 
  '("*Completions" "*Quail Completions*")
"list of the buffer names or regular expressions to be ignored by
various buffer management routines")

(defun suitable-buffer-p (buffer)
  "predicate to check the buffer exclusion from the `ignored-buffer-list'"
  (if (find-if #'(lambda (entry)
                   (string-match entry (buffer-name buffer)))
               ignored-buffer-list)
      nil
    t))

(add-to-list 'default-frame-alist `(buffer-predicate . ,#'suitable-buffer-p))

;;-------------------------------------------------------------------------------

(let ((elpa-root (expand-file-name "~/elisp/elpa")))
  (when (file-directory-p elpa-root)
    (add-to-list 'load-path elpa-root)
    (setq package-user-dir elpa-root)
    (require 'package)
    (package-initialize)))

;;-------------------------------------------------------------------------------

(let ((opt-site-lisp (expand-file-name "~/opt/share/emacs/site-lisp")))
  (when (file-directory-p opt-site-lisp)
    (add-to-list 'load-path opt-site-lisp)
    (dolist (entry (directory-files opt-site-lisp t nil t))
      (when (and (file-directory-p entry)
                 (equal 'nil (string-match "/\\.\\.?$" entry)))
        (add-to-list 'load-path (expand-file-name entry))))))

;;-------------------------------------------------------------------------------

(add-to-list 'auto-mode-alist '(".*\\.bb$" . shell-script-mode))
(add-to-list 'auto-mode-alist '("Makefile\\..*" . makefile-mode))
(add-to-list 'auto-mode-alist '(".*\\.h\\.in$" . c-mode))
(add-to-list 'auto-mode-alist '(".*\\.bat$" . dos-mode))
(add-to-list 'auto-mode-alist '("svn-.*\\.tmp$" . text-mode))

(add-hook 'text-mode-hook 'auto-fill-mode)

(add-hook 'emacs-lisp-mode-hook
          #'(lambda ()
              (setq indent-tabs-mode nil)))

;;-------------------------------------------------------------------------------

;; iswitchb
(require 'iswitchb)
(setq iswitchb-regexp t
      iswitchb-default-method 'samewindow)
(iswitchb-mode 1)

(mapcar #'(lambda (entry)
            (add-to-list 'iswitchb-buffer-ignore entry))
        ignored-buffer-list)

;;-------------------------------------------------------------------------------

(require 'whitespace)
(global-whitespace-mode 0)
(setq
 whitespace-style '(indentation space-before-tab
                                space-after-tab))

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

(require 'cperl-mode)

(fset 'perl-mode 'cperl-mode)

(add-hook 'cperl-mode-hook
          (lambda ()
            (local-set-key (kbd "C-h f") 'cperl-perldoc)
            (setq indent-tabs-mode nil)
            (cperl-set-style "C++")))

;;-------------------------------------------------------------------------------

(require 'shell)
(defun shell-jump ()
  "opens the shell in the current directory. Opens new window if
prefix argument is set"

  (interactive)
  (let ((new-shell-buf-name
         (if (eq major-mode 'dired-mode)
             (concat (buffer-name) ":shell")
           (let* ((dir (directory-file-name
                        (if buffer-file-name
                            (file-name-directory buffer-file-name)
                          default-directory))))
             (concat (car (last (split-string dir "/"))) 
                     ":shell")))))
    (if current-prefix-arg
        (let ((split-height-threshold 0)
              (split-width-threshold nil))
          (shell new-shell-buf-name))
      (let ((same-window-buffer-names
             (cons new-shell-buf-name same-window-buffer-names)))
        (shell new-shell-buf-name)))))

;;-------------------------------------------------------------------------------

(setq dired-bind-jump nil
      dired-recursive-deletes 'always
      dired-deletion-confirmer #'y-or-n-p)
(require 'dired-x)

(defadvice dired-do-shell-command
  (around split-fashion (command &optional arg file-list) 
          activate)
  "Controls the fashion of window splitting. Splits window
vertically."
  (let ((split-height-threshold 0)
        (split-width-threshold nil))
    ad-do-it))

(defadvice dired-do-async-shell-command
  (around split-fashion (command &optional arg file-list) 
          activate)
  "Controls the fashion of window splitting. Splits window
vertically."
  (let ((split-height-threshold 0)
        (split-width-threshold nil))
    ad-do-it))

(define-key dired-mode-map (kbd "M-n") #'dired-next-line)
(define-key dired-mode-map (kbd "M-p") #'dired-previous-line)
(define-key dired-mode-map (kbd "c") #'dired-do-copy)
(define-key dired-mode-map (kbd "d") #'dired-do-delete)
(define-key dired-mode-map (kbd "r") #'dired-do-rename)
(define-key dired-mode-map (kbd "M-d") #'dired-flag-file-deletion)

(put 'dired-find-alternate-file 'disabled nil)

;; -l is mandatory
;; -G omit the group name
;; -h human-readable size
(setq dired-listing-switches "-alGh")

;;-------------------------------------------------------------------------------n
(require 'info)

(define-key Info-mode-map (kbd "j") #'Info-follow-nearest-node)
(define-key Info-mode-map (kbd "M-n") nil)

;;-------------------------------------------------------------------------------
(require 'vc)

(add-to-list 'same-window-buffer-names "*vc-diff*")
(add-to-list 'same-window-regexps ".*\\.c$")

(remove-hook 'find-file-hook
             #'vc-find-file-hook)

;;-------------------------------------------------------------------------------
(require 'grep)

(add-to-list 'same-window-buffer-names "*grep*")

;;-------------------------------------------------------------------------------

(require 'server)
(setq server-log t)

(add-hook 'kill-emacs-hook #'basic-save-buffer)

;;-------------------------------------------------------------------------------

(load-conf "utils")
(load-conf "cc")
(load-conf "fonts")
(load-conf "env")

(load-conf "org" "org")
(load-conf "zb" "zenburn")
(load-conf "iresize" "iresize")
(load-conf "ac" "auto-complete")
(load-conf "root-win" "split-root")

(load-conf "wn" 'window-numbering-mode)
(load-conf "gtags" 'gtags-mode)
(load-conf "fs" 'flyspell-mode)
(load-conf "ispell" 'ispell-word)
(load-conf "psvn" 'svn-status)

(unless (string= system-type "windows-nt")
  (load-conf "wm")
  (load-conf "mpc" 'mpc)
  (load-conf "dictem" "dictem")
  (load-conf "nt" "newsticker")
  (load-conf "jabber" "emacs-jabber")
  (load-conf "wl" "wl")
  (load-conf "magit" "magit")
  (load-conf "w3m" "w3m")
  (load-conf "gdb"))

;;-------------------------------------------------------------------------------

(require 'buffer-recode)
(ring-insert evm-coding-systems-list 'windows-1251)
(ring-insert evm-coding-systems-list 'koi8-r)
(global-set-key [f5] 'recode-buffer)

;;-------------------------------------------------------------------------------
;; loaded finally to be sure that all mode maps are available

(load-conf "keys")
