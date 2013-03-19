(require 'auto-complete)
(require 'auto-complete-config)

(define-key ac-mode-map (kbd "C-c h") #'ac-last-quick-help)

(setq global-auto-complete-mode t
      ac-use-quick-help nil
      ac-comphist-file (expand-file-name "ac/comphist.dat" my-emacs-var-dir)
      ac-expand-on-auto-complete t
      ac-fuzzy-enable nil
      ac-auto-show-menu nil
      ac-auto-start 3
      ac-dictionary-directories (expand-file-name "ac/dict" my-emacs-var-dir))

(ac-set-trigger-key "TAB")

(defun my-cmode-ac-hook ()
  (setq ac-sources (if gtags-mode
      '(ac-source-words-in-buffer ac-source-dictionary ac-source-gtags)
    '(ac-source-words-in-buffer ac-source-dictionary))))

(add-hook 'c-mode-common-hook
	  #'my-cmode-ac-hook t)

(defun my-lisp-ac-hook ()
  (setq ac-sources '(ac-source-symbols
		     ac-source-variables
		     ac-source-functions)))

(add-hook 'emacs-lisp-mode-hook
	  #'my-lisp-ac-hook t)

(global-auto-complete-mode 1)