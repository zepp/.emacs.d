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
 gdb-many-windows t
 calculator-electric-mode nil
 browse-url-browser-function 'browse-url-firefox)

(put 'scroll-left 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(add-to-list 'warning-suppress-types '(undo discard-info))

(add-to-list 'same-window-buffer-names "*grep*")

(setenv "GPG_AGENT_INFO" nil)

(defvar ignored-buffer-list
  '("\\*Completions" "\\*Quail Completions\\*" "\\*Backtrace\\*" "\\*magit-edit-log\\*"
    "\\*P4" "\\*Buffer List\\*" "\\**Shell Command Output\\*" "\\*helm mini\\*")
  "list of the buffer names or regular expressions to be ignored by
various buffer management routines")

;;-------------------------------------------------------------------------------

(defun suitable-buffer-p (buffer)
  "predicate to check the buffer exclusion from the `ignored-buffer-list'"
  (if (find-if #'(lambda (entry)
                   (string-match entry (buffer-name buffer)))
               ignored-buffer-list)
      nil
    t))

(setf frame-title-format "%F")
(add-to-list 'default-frame-alist `(buffer-predicate . ,#'suitable-buffer-p))
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))

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

(mapcar #'(lambda (entry)
            (add-to-list 'ido-ignore-buffers entry))
        ignored-buffer-list)

;;-------------------------------------------------------------------------------

(require 'whitespace)

(global-whitespace-mode 1)
(setq whitespace-style '(face
                         trailing
                         empty
                         indentation
                         space-after-tab
                         space-before-tab))

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

;;-------------------------------------------------------------------------------

(require 'server)
(add-hook 'kill-emacs-hook #'basic-save-buffer)

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

(require 'local-env nil t)
(require 'my-utils)

(load-conf 'iresize
           '(global-set-key (kbd "C-c r") #'iresize-mode)
           t)

(load-conf 'window-numbering
           '(window-numbering-mode 1)
           t)

(load-conf 'expand-region
           '(global-set-key (kbd "M-@") 'er/expand-region)
           t)

(load-conf 'which-key
           '(progn
              (setq which-key-idle-delay 2.0)
              (which-key-mode 1))
           t)

(load-conf 'helm "helm" t)
(load-conf 'helm-gtags "helm-gtags" t)
(load-conf 'auto-complete "ac" t)
(load-conf 'aggressive-indent "ai" t)
(load-conf 'flyspell "fs" t)

(load-conf 'dired "dired")
(load-conf 'cc-mode "cc")
(load-conf 'vc "vc")

(load-conf 'elscreen "es")
(load-conf 'erc "erc")
(load-conf 'jabber "jabber")
(load-conf 'org "org")

;;-------------------------------------------------------------------------------

;; to make a cursor navigation a little bit easy
(global-set-key (kbd "M-n") #'forward-paragraph)
(global-set-key (kbd "M-p") #'backward-paragraph)

;; buffer related shortcuts start from C-x
(global-set-key (kbd "C-x p") #'previous-buffer)
(global-set-key (kbd "C-x n") #'next-buffer)
(global-set-key (kbd "C-x l")
                (lexical-let (swap-last)
                  #'(lambda ()
                      "that's a wrapper around the `swap-buffers'
function to keep a state variable"
                      (interactive)
                      (swap-buffers swap-last)
                      (setq swap-last (not swap-last)))))
(global-set-key (kbd "C-x d") #'dired-jump)
(global-set-key (kbd "C-x c") #'shell-jump)
(global-set-key (kbd "C-x C-d") #'dired)
(global-set-key (kbd "C-x M-d") #'dired-other-window)
(global-set-key (kbd "C-x M-f") #'find-file-at-point)
(global-set-key (kbd "C-x M-b") #'switch-to-buffer-other-window)
(global-set-key (kbd "C-x C-n")
                #'(lambda (newname)
                    (interactive
                     (list (read-string "Rename current buffer to: "
                                        (buffer-name (current-buffer)))))
                    (rename-buffer newname)))
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

(global-set-key (kbd "C-c C-x C-a") #'org-agenda)
