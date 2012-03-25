(require 'psvn)

(setq svn-status-hide-unmodified t
      svn-status-hide-externals t)

(add-hook 'svn-log-edit-mode-hook '(lambda () (flyspell-mode)))
