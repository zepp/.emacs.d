(require 'cc-mode)

;; http://www.linux.org.ru/books/GNU/emacs/emacs_27.html
;; Create my personal coding style.

(c-add-style "pavels-template"
             '((indent-tabs-mode . nil)
               (c-basic-offset . 2)
               (c-hanging-braces-alist .
                                       ((substatement-open
                                         before after)))
               (c-offsets-alist .
                                ((topmost-intro . 0)
                                 (substatement . +)
                                 (substatement-open . 0)
                                 (case-label . +)
                                 (arglist-cont-nonempty . ++)
                                 (label . -)
                                 (inclass . +)
                                 (access-label . -)))))

(setq c-default-style
      '((other . "pavels-template")))

(add-hook 'c-mode-common-hook
          '(lambda()
             (auto-fill-mode t)
             ;; (c-toggle-electric-state 1) enabled by auto-newline
             ;; (c-toggle-auto-newline 1)
             ;; (c-toggle-hungry-state 1)
             (c-toggle-auto-hungry-state 1)))
