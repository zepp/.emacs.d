(require 'flyspell)

(setq
 flyspell-delay 1
 flyspell-default-dictionary ispell-dictionary)

(define-key flyspell-mode-map (kbd "C-c i") #'ispell-change-dictionary)
