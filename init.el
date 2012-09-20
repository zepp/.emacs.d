;;(setq max-lisp-eval-depth (* 1024 1024))

(transient-mark-mode 1)
(menu-bar-mode 0)
(tool-bar-mode 0)
(toggle-scroll-bar -1)
(column-number-mode 1)

(setq inhibit-startup-screen t
      x-select-enable-clipboard t
      frame-title-format "%F")

(put 'scroll-left 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(require 'warnings)
(add-to-list 'warning-suppress-types '(undo discard-info))

(add-to-list 'load-path "~/.emacs.d/loadable/")

(setq my-emacs-var-dir "~/.emacs.var"
      my-emacs-personal-cfg (expand-file-name "personal" user-emacs-directory))

(defun load-ext (cfg &optional name)

  (defun do-load (cfg)
    (load (expand-file-name (format "cfg/%s.el" cfg)
			    user-emacs-directory))
    (let ((path (expand-file-name (concat cfg ".el")
				  my-emacs-personal-cfg)))
      (if (file-regular-p path)
          (load path)
        nil)))

  (if name
      (if (find-if #'(lambda (path)
		       (find-if #'(lambda (dir)
				    (equal 0 (search name dir)))
				(nreverse (split-string
					   (directory-file-name path) "/"))))
		   load-path)
	  (do-load cfg))
    (do-load cfg)))

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
            (cperl-set-style "C++")
            (define-key cperl-mode-map (kbd "C-h f") 'cperl-perldoc)))

;;-------------------------------------------------------------------------------

(require 'cl)
(setq auto-mode-alist (acons ".*\\.bb$" 'shell-script-mode auto-mode-alist))
(setq auto-mode-alist (acons "Makefile\\..*" 'makefile-mode auto-mode-alist))
(setq auto-mode-alist (acons ".*\\.h\\.in$" 'c-mode auto-mode-alist))
(setq auto-mode-alist (acons ".*\\.bat$" 'dos-mode auto-mode-alist))

;;-------------------------------------------------------------------------------

(require 'eshell)
(defun eshell-jump ()
  (interactive)
  (let ((eshell-buffer-name 
	 (if (eq major-mode 'dired-mode)
	     (concat "eshell-" (buffer-name))
	   (let* ((dir (directory-file-name
			(if buffer-file-name 
			    (file-name-directory buffer-file-name)
			  default-directory))))
	     (concat "eshell-" 
		     (car (last (split-string dir "/"))))))))
    (unless (get-buffer eshell-buffer-name)
      (add-to-list 'same-window-buffer-names eshell-buffer-name))
    (eshell)))

;;-------------------------------------------------------------------------------

(setq dired-bind-jump nil)
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

;;-------------------------------------------------------------------------------

(let ((elpa-root (expand-file-name "~/elisp/elpa")))
  (when (file-directory-p elpa-root)
    (add-to-list 'load-path elpa-root)
    (setf package-user-dir elpa-root)
    (require 'package)
    (package-initialize)))

;;-------------------------------------------------------------------------------

;; external packages or big configuration statements should be moved
;; to separate file
(load-ext "server")
(load-ext "cc")
(load-ext "fonts")
(load-ext "env")
(load-ext "org")
(load-ext "gtags" "global")
(load-ext "prefs" "prefs")
(load-ext "zencolor" "zenburn")
(load-ext "fs")
(load-ext "grep")
(load-ext "psvn" "psvn")
(load-ext "sr" "sunrise")
(load-ext "iresize" "iresize")
(load-ext "wn" "window-numbering")

(when (not (string= system-type "windows-nt"))
  (load-ext "mpc")
  (load-ext "dictem" "dictem")
  (load-ext "nt" "newsticker")
  (load-ext "jabber" "emacs-jabber")
  (load-ext "wl" "wl")
  (load-ext "magit" "magit")
  (load-ext "w3m" "w3m")
  (load-ext "gdb"))

;;-------------------------------------------------------------------------------

(load "my-utils")
(load "buffer-recode")
(load "wm")
(ring-insert evm-coding-systems-list 'windows-1251)
(ring-insert evm-coding-systems-list 'koi8-r)
(global-set-key [f5] 'recode-buffer)

;;-------------------------------------------------------------------------------
;; loaded finally to be sure that all mode maps are available

(load-ext "keys")

;;-------------------------------------------------------------------------------

(when (not (string= system-type "windows-nt"))
  (add-to-list 'initial-frame-alist '(name . "emacs-initial"))
  (add-to-list 'default-frame-alist '(name . "emacs-client"))
  (add-to-list 'default-frame-alist '(font . "terminus-14"))
  (add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
  (when (functionp 'newsticker-start-ticker)
    (newsticker-start-ticker))
  (when (functionp 'wl)
    (wl 1)))
