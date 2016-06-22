(require 'cl)
(require 'warnings)

(menu-bar-mode 0)
(tool-bar-mode 0)
(toggle-scroll-bar -1)
(column-number-mode 1)

(transient-mark-mode 1)
(delete-selection-mode 1)

(electric-pair-mode 1)

(setq
 inhibit-startup-screen t
 visible-bell t
 make-pointer-invisible t
 x-select-enable-clipboard t
 frame-title-format "%F"
 default-input-method 'russian-computer
 gdb-many-windows t
 calculator-electric-mode nil)

(put 'scroll-left 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))

(add-to-list 'warning-suppress-types '(undo discard-info))

(add-to-list 'same-window-buffer-names "*grep*")

;;-------------------------------------------------------------------------------

(defun load-conf (conf &optional file-sym req)

  (defun do-load (conf)
    (let ((main  (expand-file-name (format "conf.d/%s.el" conf) user-emacs-directory))
          (local (expand-file-name (format "local.d/%s.el" conf) user-emacs-directory)))
      (if (file-regular-p local)
          `(progn (load ,main) (load ,local))
        `(load ,main))))

  (if (not file-sym)
      (eval (do-load conf))
    (progn
      (eval-after-load file-sym (do-load conf))
      (when req
        (require file-sym nil t)))))

(defun load-simple (file-sym form)
  (eval-after-load file-sym form)
  (require file-sym nil t))

;;-------------------------------------------------------------------------------

(defvar ignored-buffer-list 
  '("\\*Completions" "\\*Quail Completions\\*" "\\*Backtrace\\*" "\\*magit-edit-log\\*"
    "\\*P4" "\\*Buffer List\\*" "\\**Shell Command Output\\*" "\\*helm mini\\*")
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
;; package management

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://stable.melpa.org/packages/")
   t)
  (package-initialize))

(let ((my-load-path (expand-file-name "~/.emacs.d/loadable/")))
  (add-to-list 'load-path my-load-path)
  (dolist (entry (directory-files my-load-path t nil t))
    (when (and (file-directory-p entry)
               (equal 'nil (string-match "/\\.\\.?$" entry)))
      (add-to-list 'load-path (expand-file-name entry)))))

;;-------------------------------------------------------------------------------

(add-to-list 'auto-mode-alist '(".*\\.bb$" . shell-script-mode))
(add-to-list 'auto-mode-alist '("Makefile\\..*" . makefile-mode))
(add-to-list 'auto-mode-alist '(".*\\.mak$" . makefile-mode))
(add-to-list 'auto-mode-alist '(".*\\.h\\.in$" . c-mode))
(add-to-list 'auto-mode-alist '(".*\\.bat$" . dos-mode))
(add-to-list 'auto-mode-alist '("svn-.*\\.tmp$" . text-mode))

;;-------------------------------------------------------------------------------

(global-visual-line-mode 1)
(setq visual-line-fringe-indicators '(nil right-curly-arrow))

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
;; diff

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

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      uniquify-ignore-buffers-re "^\\*")

;;-------------------------------------------------------------------------------
;; spell

(setq ispell-program-name "aspell"
      ispell-have-new-look t
      ispell-dictionary "english"
      ispell-extra-args '("--sug-mode=ultra"))

(when (boundp flyspell-mode)
  (add-hook 'text-mode-hook #'flyspell-mode))

;;-------------------------------------------------------------------------------

(require 'server)
(add-hook 'kill-emacs-hook #'basic-save-buffer)

;;-------------------------------------------------------------------------------

(require 'iresize nil t)

(load-simple 'window-numbering
             '(window-numbering-mode 1))

(load-simple 'expand-region
             '(global-set-key (kbd "M-@") 'er/expand-region))

(load-simple 'which-key
             '(progn (setq which-key-idle-delay 2.0)
                     (which-key-mode 1)))

;;-------------------------------------------------------------------------------

(load-conf "utils")
(load-conf "fonts")

(load-conf "helm" 'helm t)
(load-conf "helm-gtags" 'helm-gtags t)
(load-conf "ac" 'auto-complete t)
(load-conf "dired" 'dired t)
(load-conf "ai" 'aggressive-indent t)

(load-conf "fs" 'flyspell)
(load-conf "cc" 'cc-mode)
(load-conf "vc" 'vc)

(load-conf "es" 'elscreen)
(load-conf "erc" 'erc)
(load-conf "jabber" 'jabber)
(load-conf "org" 'org)

(load-conf "env")

(load-conf "keys")
