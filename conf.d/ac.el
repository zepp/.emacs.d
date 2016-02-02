;; * After selecting candidates, TAB will behave as RET
;; * TAB will behave as RET only on candidate remains
(setq ac-dwim nil)

(define-key ac-mode-map (kbd "M-t") #'ac-expand)

(setq global-auto-complete-mode t
      ac-use-quick-help nil
      ac-expand-on-auto-complete t
      ac-fuzzy-enable nil
      ac-auto-show-menu nil
      ac-auto-start 3)

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
