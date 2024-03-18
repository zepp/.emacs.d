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
 default-input-method 'russian-computer
 browse-url-browser-function 'browse-url-chrome
 browse-url-chrome-program "brave"
 custom-file "~/.emacs.d/local.d/custom.el")

(put 'scroll-left 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(add-to-list 'warning-suppress-types '(undo discard-info))

(add-to-list 'same-window-buffer-names "*grep*")

(add-hook 'kill-emacs-hook #'basic-save-buffer)

(setenv "GPG_AGENT_INFO" nil)

(defvar ignored-buffer-list
  '("\\*Completions" "\\*Quail Completions\\*" "\\*Backtrace\\*" "\\*magit-edit-log\\*" "\\*P4" "\\*Buffer List\\*")
  "list of the buffer names or regular expressions to be ignored by
various buffer management routines")

;;-------------------------------------------------------------------------------

(defun suitable-buffer-p (buffer)
  "predicate to check the buffer exclusion from the `ignored-buffer-list'"
  (not (find-if #'(lambda (entry)
                    (string-match entry (buffer-name buffer)))
                ignored-buffer-list)))

(setf frame-title-format "%F")
(add-to-list 'default-frame-alist `(buffer-predicate . ,#'suitable-buffer-p))
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))

;;-------------------------------------------------------------------------------

(add-to-list 'auto-mode-alist '("Makefile\\..*" . makefile-mode))
(add-to-list 'auto-mode-alist '(".*\\.mak$" . makefile-mode))
(add-to-list 'auto-mode-alist '(".*\\.h\\.in$" . c-mode))

;;-------------------------------------------------------------------------------

(global-visual-line-mode 1)
(setq visual-line-fringe-indicators '(nil right-curly-arrow))

;;-------------------------------------------------------------------------------

(add-hook 'text-mode-hook
          #'(lambda()
              (flyspell-mode 1)))

;;-------------------------------------------------------------------------------
;; e/common lisp

(defun lisp-no-tabs()
  "it disables tabs indentation"
  (setq indent-tabs-mode nil))

(add-hook 'emacs-lisp-mode-hook #'lisp-no-tabs)
(add-hook 'lisp-mode-hook #'lisp-no-tabs)

;;-------------------------------------------------------------------------------

(global-whitespace-mode 1)
(setq whitespace-style '(face
                         trailing
                         space-after-tab
                         space-before-tab))

;;-------------------------------------------------------------------------------
;; diff

(add-hook 'diff-mode-hook
          '(lambda ()
             ;; diff-goto-source
             (define-key diff-mode-map (kbd "C-m") 'diff-goto-source)))

;;-------------------------------------------------------------------------------

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      uniquify-ignore-buffers-re "^\\*")

;;-------------------------------------------------------------------------------
;; package management

(let ((default-directory "~/.emacs.d/loadable/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(require 'my-utils)

;;-------------------------------------------------------------------------------

(require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://stable.melpa.org/packages/")
   t)
  (package-initialize)

(load-package 'expand-region
              :after-load '((global-set-key (kbd "M-@") #'er/expand-region))
              :required t)

(load-package 'which-key
              :options '((which-key-idle-delay . 2.0))
              :after-load '((which-key-mode 1))
              :required t)

(load-package 'company
              :after-load '((global-company-mode 1)
                            (global-set-key (kbd "M-t") #'company-complete))
              :required t)

(load-package 'ivy :config "ivy")

(load-package 'powerline
              :after-load '((powerline-default-theme))
              :required t)

(load-package 'tide
              :config "tide")

;; embedded packages
(load-package 'ispell :config "ispell")
(load-package 'org :config "org")
(load-package 'dired :config "dired")
(load-package 'ido
              :options '((ido-enable-flex-matching . t)
                        (ido-create-new-buffer . 'always))
              :after-load '((dolist (entry ignored-buffer-list)
                             (add-to-list 'ido-ignore-buffers entry))))

;;-------------------------------------------------------------------------------
(global-set-key (kbd "C-x p") #'previous-buffer)
(global-set-key (kbd "C-x c") #'shell-jump)
(global-set-key (kbd "C-x M-f") #'find-file-at-point)
(global-set-key (kbd "C-x C-x") #'server-edit)

;; general commands start from C-c
(global-set-key (kbd "C-c g") #'rgrep)
(global-set-key (kbd "C-c w") #'browse-url)
;; window management in StumpWM style :)
(global-set-key (kbd "C-c s") #'split-window-horizontally)
(global-set-key (kbd "C-c v") #'split-window-vertically)
(global-set-key (kbd "C-c q") #'delete-other-windows)
(global-set-key (kbd "C-c k") #'delete-window)
(global-set-key (kbd "C-c o") #'other-window)

(global-set-key (kbd "C-x C-a") #'org-agenda)

(load custom-file :noerror)
