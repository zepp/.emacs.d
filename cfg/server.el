(setq server-log t)
(require 'server)

(add-hook 'kill-emacs-hook #'(lambda ()
			       (with-current-buffer "*Messages*"
				 (setq buffer-file-name
				       (expand-file-name "messages"
							 my-emacs-var-dir))
				 (basic-save-buffer))))
