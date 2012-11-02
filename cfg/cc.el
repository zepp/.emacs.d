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

(mapcar
 #'(lambda (h)
     (add-hook 'c-mode-common-hook h))
 '(auto-fill-mode c-toggle-hungry-state visual-line-mode))

(add-hook 'c-mode-common-hook
          '(lambda()
	     (define-key c-mode-base-map [remap complete-symbol] 'dabbrev-completion)
	     (c-toggle-auto-newline -1)) t)
