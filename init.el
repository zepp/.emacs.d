;; -*- lexical-binding: t; -*-

(setq
 calendar-week-start-day 1
 default-input-method 'russian-computer
 custom-file (locate-user-emacs-file "custom.el"))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; prevent warning buffer from popping up in case of
;; `undo-outer-limit' is exceeded
(require 'warnings)
(add-to-list 'warning-suppress-types '(undo discard-info))

(add-hook 'kill-emacs-hook #'basic-save-buffer)

;;-------------------------------------------------------------------------------
;; package management

(defun pavel/emacs-29-p ()
  "Predicate to check Emacs version"

  (string-match "29\\.[0-9]+" emacs-version))

(let* ((loadable-directory "~/.emacs.d/loadable/")
       (pavel-autoloads (expand-file-name
			 "pavel-autoloads.el"
			 loadable-directory)))
  (normal-top-level-add-to-load-path `(,loadable-directory))
  (loaddefs-generate loadable-directory pavel-autoloads)
  (require 'pavel-autoloads))

(setq  use-package-always-defer t
       use-package-hook-name-suffix nil)
(require 'package)
(setq use-package-compute-statistics t)
(add-to-list
 'package-archives
 '("melpa" . "https://stable.melpa.org/packages/")
 t)
(package-initialize)

;;-------------------------------------------------------------------------------
;; UI related

(setq
 inhibit-startup-screen t
 frame-title-format "%b"
 visible-bell t
 visual-line-fringe-indicators '(nil right-curly-arrow))

(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))

(tool-bar-mode -1)
(toggle-scroll-bar -1)
(column-number-mode 1)

(use-package windmove
  :bind (:map ctl-x-map
              ("<up>" . windmove-up)
              ("<right>" . windmove-right)
              ("<down>" . windmove-down)
              ("<left>" . windmove-left))
  :demand t)

;; It replicates a common keybinding to switch between windows on different
;; OS. It must be placed here to prevent overriding by `tab-bar-mode'.
(global-set-key [(control tab)] #'other-window)

(use-package tab-bar
  :bind (:map tab-prefix-map
              ("t" . tab-new)
              ("M-t" . other-tab-prefix)
              ("k" . tab-close)
              ("<left>" . tab-previous)
              ("<right>" . tab-next)
              ("M-<left>" . tab-bar-move-tab-backward)
              ("M-<right>" . tab-bar-move-tab))

  :config
  (setq tab-bar-new-tab-choice "*scratch*")
  (tab-bar-mode 1)

  :demand t)

;; buffer and window management
(setq switch-to-buffer-obey-display-actions t)

;; some of this modes provide navigation capabilities
(add-to-list 'display-buffer-alist
             '((or (derived-mode . compilation-mode)
                   (major-mode . occur-mode)
                   (major-mode . xref--xref-buffer-mode))
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (side . bottom)
               (window-min-height . 0.25)))

(when (pavel/emacs-29-p)
  (define-key ctl-x-map (kbd "w d") #'pavel/toggle-window-dedicated))
(define-key ctl-x-map (kbd "M-b") #'switch-to-buffer-other-window)
(define-key ctl-x-map (kbd "M-f") #'find-file-other-window)
(define-key ctl-x-map (kbd "M-0") #'quit-window)

;; buffer management
(define-key ctl-x-x-map (kbd "o") #'next-buffer)
(define-key ctl-x-x-map (kbd "M-o") #'previous-buffer)

(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'forward
      uniquify-ignore-buffers-re "^\\*"))

;; basic completion that popups bottom window
(setq completion-styles '(basic substring)
      ;; help message in *Completions* buffer.
      completion-show-help nil
      completion-auto-select 'second-tab
      ;; option to control a completion window visibility
      completion-auto-help 'visible
      completion-ignore-case t
      ;; `veritcal' is interesting alternative option but next/prev commands do
      ;; not respect direction, also `vertical' does not match perfectlry with
      ;; `completions-detailed'
      completions-format 'one-column
      completions-max-height 10)

;; similar to isearch
(define-key completion-list-mode-map (kbd "M-e") #'switch-to-minibuffer)

(unless (pavel/emacs-29-p)
  (use-package completion-preview
    :bind (:map completion-preview-active-mode-map
                ("M-p" . completion-preview-prev-candidate)
                ("M-n" . completion-preview-next-candidate)
                ("M-i" . completion-preview-insert))
    :config
    ;; TODO: refile to `pavel-org-conf' after a full migration to Emacs 3X
    (add-to-list 'completion-preview-commands 'org-self-insert-command)

    :hook
    (comint-mode-hook . completion-preview-mode)
    (eshell-mode-hook . completion-preview-mode)
    (text-mode-hook . completion-preview-mode)

    :demand t))

;; alternative completion that uses minibuffer
(use-package ido
  :init
  (setq ido-enable-flex-matching t
        ido-cannot-complete-command 'ido-next-match)

  :config
  (dolist (buf '("\\*Quail Completions\\*"
                 "\\*Completions\\*"
                 "\\*Buffer List\\*"))
    (add-to-list 'ido-ignore-buffers buf))

  (ido-mode 1)

  :demand t)

(use-package which-key
  :init (setq which-key-idle-delay 2.0)
  :config (which-key-mode 1)
  :demand t
  :ensure t)

(use-package isearch
  :bind
  (:map isearch-mode-map
        ("M-o" . isearch-occur))
  (:map search-map
        ;; remapped to be handy and mnemonic
        ("s" . isearch-forward-symbol-at-point)
        ("t" . isearch-forward-thing-at-point)
        ;; reserved
        ;;
        ;; "r" – find a symbol references, "j" – jump to a symbol definition
        ;;
        ;; extra commands
        ("g" . search-scope-grep)
        ;; common key, infrequently used
        ("%" . search-scope-replace))
  :init
  (setq isearch-allow-motion t
        isearch-motion-changes-direction t)

  :demand t)

;;-------------------------------------------------------------------------------

;; disable tab indentation globally
(setq-default indent-tabs-mode nil)

(save-place-mode 1)
(transient-mark-mode 1)
(delete-selection-mode 1)
(electric-pair-mode 1)

;;-------------------------------------------------------------------------------

(defun pavel/compile-buf-name-advice (orig &rest args)
  "it advices `compile' to prettify name of a compilation buffer and
make it more informative"

  ;; let's take command string into account
  (let* ((command (string-trim (nth 0 args)))
         (chopped (if (> (length command) 24)
                      (substring command 0 24)
                    command))
         (ellipsis (if (string= command chopped)
                       "" "…"))
         (compilation-buffer-name-function
          #'(lambda (mode)
              (format "*%s: %s%s*" mode chopped ellipsis))))
    (apply orig args)))

(use-package compile
  :config
  (let ((alist
         '((tsl
            "^\\[tsl\\]\\s-+ERROR in \\([^(\r\n]+\\)(\\([0-9]+\\),\\([0-9]+\\))?$" 1 2 3)
           (angular-nx
            "^\\s-*\\([^:\r\n]+\\):\\([0-9]+\\):\\([0-9]+\\):$" 1 2 3))))
    (dolist (cell alist)
      (add-to-list 'compilation-error-regexp-alist-alist cell)
      (add-to-list 'compilation-error-regexp-alist (car cell))))

  (advice-add 'compile :around #'pavel/compile-buf-name-advice))

;;-------------------------------------------------------------------------------

(keymap-global-unset "M-q") ;; fill-paragraph
(define-key ctl-x-map (kbd "M-s") #'write-file)

(require 'pavel-text-conf)
(require 'pavel-fm-conf)

;;-------------------------------------------------------------------------------
;; os specific configuration

(cond

 ((string= system-type "windows-nt")
  ;; it looks ugly
  (menu-bar-mode -1)

  (setq
   ;; make `rgrep' work
   find-program
   (expand-file-name "bin/find.exe"
                     (getenv "ChocolateyInstall")))

  ;; make `hunspell' work
  (let ((root (expand-file-name ".dicts"
                                (or (getenv "HOME")
                                    (getenv "HOMEPATH")))))
    (setenv "DICPATH" root)
    (setenv "DICTIONARY" "en_US")
    (setq ispell-hunspell-dict-paths-alist (pavel/list-dicts root))))

 ((string= system-type "darwin")
  (setq mac-command-modifier 'meta
        mac-option-modifier 'none
        browse-url-browser-function 'browse-url-default-macosx-browser
        visible-bell nil)
  (when (eq window-system 'ns)
    (use-package exec-path-from-shell
      :config
      (add-to-list 'exec-path-from-shell-variables "LANG")
      (exec-path-from-shell-initialize)
      :demand t
      :ensure t))))

;;-------------------------------------------------------------------------------

(load custom-file :noerror)
