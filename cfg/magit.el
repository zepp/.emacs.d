(autoload 'magit-status "magit" nil t)

(require 'magit)

(add-hook 'magit-log-edit-mode-hook #'flyspell-mode)
