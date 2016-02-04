(require 'auto-complete-config)

(setf ac-use-quick-help nil
      ac-dwim                    t   ; after selecting candidates, TAB will behave as RET
      ac-expand-on-auto-complete t   ; whether or not to expand a common part of whole candidates
      ac-ignore-case             nil ; 'smart
      ac-auto-show-menu          nil
      ac-auto-start              nil ; should be nil to use trigger key
      )

(ac-set-trigger-key "TAB")

(defun ac-fix-c-sources ()
  (setq ac-sources '(ac-source-gtags
                     ac-source-words-in-buffer
                     ac-source-dictionary)))

(add-hook 'c-mode-common-hook
          #'ac-fix-c-sources t)

(defun ac-fix-lisp-sources ()
  (setq ac-sources '(ac-source-symbols
                     ac-source-variables
                     ac-source-functions)))

(add-hook 'emacs-lisp-mode-hook
          #'ac-fix-lisp-sources t)

(global-auto-complete-mode 1)
