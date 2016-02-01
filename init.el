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
 inhibit-startup-screen t
 visible-bell t
 make-pointer-invisible t
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

(defun load-conf (conf &optional file-sym req)

  (defun do-load (conf)
    (let ((main  (expand-file-name (format "conf.d/%s.el" conf)
                                   user-emacs-directory))
          (local (expand-file-name (format "%s.el" conf)
                                   local-conf-dir)))
      (if (file-regular-p local)
          `(progn (load ,main) (load ,local))
        `(load ,main))))

  (if file-sym
      (eval-after-load file-sym (do-load conf))
    (eval (do-load conf)))
  (when (and req file-sym)
    (require file-sym nil t)))

;;-------------------------------------------------------------------------------

(defvar ignored-buffer-list 
  '("*Completions" "*Quail Completions*" "*magit-edit-log*")
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
    ;; installation script places package.el into to the elpa-root
    (if (< emacs-major-version 24)
        (add-to-list 'load-path elpa-root))
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
(add-to-list 'auto-mode-alist '(".*\\.mak$" . makefile-mode))
(add-to-list 'auto-mode-alist '(".*\\.h\\.in$" . c-mode))
(add-to-list 'auto-mode-alist '(".*\\.bat$" . dos-mode))
(add-to-list 'auto-mode-alist '("svn-.*\\.tmp$" . text-mode))
(when (fboundp 'org-mode)
  (add-to-list 'auto-mode-alist '("\\.org\\(-mode\\)?$" . org-mode)))

(add-hook 'text-mode-hook 'auto-fill-mode)
(when (boundp flyspell-mode)
  (add-hook 'text-mode-hook #'flyspell-mode))

(when (fboundp 'gtags-mode)
  (add-hook 'c-mode-common-hook #'gtags-mode))

;;-------------------------------------------------------------------------------
;; e/common lisp

(defun lisp-no-tabs()
  "it disables tabs indentation"
  (setq indent-tabs-mode nil))

(add-hook 'emacs-lisp-mode-hook #'lisp-no-tabs)
(add-hook 'lisp-mode-hook #'lisp-no-tabs)

;;-------------------------------------------------------------------------------
;; ido

(require 'ido)
(ido-mode 1)

(mapcar #'(lambda (entry)
            (add-to-list 'ido-ignore-buffers entry))
        ignored-buffer-list)

;;-------------------------------------------------------------------------------

(require 'whitespace)
(global-whitespace-mode 0)
(setq
 whitespace-style '(indentation space-before-tab
                                space-after-tab))

;;-------------------------------------------------------------------------------
;; calculator

(setq calculator-electric-mode nil)

;;-------------------------------------------------------------------------------
;; compile

(setq compilation-error-regexp-metaware
      '("[Ew] \"\\(.*\\)\",L\\([0-9]+\\)/C\\([0-9]+\\)\\((#[0-9]+)\\)?:\t\
\\(.*\\)" 1 2 3)
      compilation-error-regexp-alist
      `(,compilation-error-regexp-metaware gnu gcc-include))

;;-------------------------------------------------------------------------------
;; diff/ediff

(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function #'(lambda (&optional arg)
                                      (if (> (frame-width) 140)
                                          (split-window-horizontally arg)
                                        (split-window-vertically arg))))
(add-hook 'diff-mode-hook
          '(lambda ()
             ;; diff-goto-source
             (define-key diff-mode-map (kbd "C-m") 'diff-goto-source)))

;;-------------------------------------------------------------------------------

(fset 'perl-mode 'cperl-mode)

(add-hook 'cperl-mode-hook
          (lambda ()
            (local-set-key (kbd "C-h f") 'cperl-perldoc)
            (setq indent-tabs-mode nil)
            (cperl-set-style "C++")))

;;-------------------------------------------------------------------------------
;; shell

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
      (let (pop-up-windows
            (same-window-buffer-names
             (cons new-shell-buf-name same-window-buffer-names)))
        (shell new-shell-buf-name)))))

;;-------------------------------------------------------------------------------
;; info, useless under non UNIX platform

(eval-after-load 'info
  '(progn 
     (define-key Info-mode-map (kbd "j") #'Info-follow-nearest-node)
     (define-key Info-mode-map (kbd "M-n") nil)))

;;-------------------------------------------------------------------------------
;; unfortunately there is no autoloads so I have to do it in such way
(require 'dictem nil t)

(setq dictem-default-database "gcide"
      dictem-use-existing-buffer t)

;;-------------------------------------------------------------------------------
;; psvn

(setq svn-status-hide-unmodified t
      svn-status-hide-externals t)

;;-------------------------------------------------------------------------------
;; gdb

(setq gdb-many-windows t)

;;-------------------------------------------------------------------------------

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)

;;-------------------------------------------------------------------------------
;; missing in fresh installation
(require 'window-numbering nil t)

(eval-after-load 'window-numbering '(window-numbering-mode 1))

;;-------------------------------------------------------------------------------
(require 'expand-region nil t)

(eval-after-load 'expand-region '(global-set-key (kbd "M-@") 'er/expand-region))

;;-------------------------------------------------------------------------------
;; missing in older Emacs versions
(require 'color-theme nil t)

(eval-after-load 'color-theme 
  '(when (fboundp 'color-theme-initialize)
     (color-theme-initialize)))

;;-------------------------------------------------------------------------------
;; ispell

(setq ispell-program-name "aspell"
      ispell-have-new-look t
      ispell-dictionary "english"
      ispell-extra-args '("--sug-mode=ultra"))

;;-------------------------------------------------------------------------------
;; missing in fresh installation
(require 'iresize nil t)

(eval-after-load 'iresize
  '(progn 
     (define-key iresize-mode-map (kbd "RET") #'iresize-mode)
     (define-key iresize-mode-map (kbd "C-f") #'enlarge-window-horizontally)
     (define-key iresize-mode-map (kbd "C-b") #'shrink-window-horizontally)))

;;-------------------------------------------------------------------------------
;; grep

(add-to-list 'same-window-buffer-names "*grep*")

;;-------------------------------------------------------------------------------
(require 'server)
(setq server-log t)

(add-hook 'kill-emacs-hook #'basic-save-buffer)

;;-------------------------------------------------------------------------------

(load-conf "utils")
(load-conf "fonts")
(load-conf "env")

(load-conf "ac" 'auto-complete t)
(load-conf "dired" 'dired t)
(load-conf "es" 'elscreen)

(load-conf "gtags" 'gtags)
(load-conf "fs" 'flyspell)
(load-conf "cc" 'cc-mode)
(load-conf "vc" 'vc)

(load-conf "erc" 'erc)

;;-------------------------------------------------------------------------------
;; some top-level wrappers

(defun bitlbee ()
  "Starts the ERC connection to the local bitlbee server using
secrets file"

  (interactive)
  (unless (get-buffer "&bitlbee")
    (require 'secrets)
    (erc :server bitlbee-server
         :nick bitlbee-nick 
         :password bitlbee-password)))

(defun jabber-with-secrets ()
  "Starts the jabber session using secrets file"

  (interactive)
  (unless (boundp 'jabber-roster-buffer)
    (require 'secrets)
    (jabber-connect-all)
    (switch-to-buffer jabber-roster-buffer)))

;;-------------------------------------------------------------------------------
;; global key bindings

(load-conf "keys")
