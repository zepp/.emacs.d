(require 'flyspell)

(setq
 flyspell-delay 1
 flyspell-default-dictionary ispell-dictionary)

(define-key flyspell-mode-map (kbd "C-c i") #'ispell-change-dictionary)

(add-hook 'flyspell-mode-hook
          #'(lambda ()
              (when flyspell-mode
                (flyspell-buffer))))

(add-hook 'text-mode-hook #'flyspell-mode)
