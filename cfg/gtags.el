;; depends on cc-mode because c-mode-map defined there
(require 'cc-mode)

(setq gtags-suggested-key-mapping t)

(require 'gtags)

(define-key c-mode-base-map (kbd "M-,")   'gtags-find-tag-from-here)
(define-key c-mode-base-map (kbd "C-M-,") 'gtags-find-rtag)
(define-key c-mode-base-map (kbd "M-/")   'gtags-find-pattern)

(defun gtags-update-current-file (root)
  (interactive)
  (let ((rel-file-name (substring
			(buffer-file-name
			 (current-buffer)) (length root)))
	(default-directory root))
    (start-process "update-gtags" "update-gtags"
		   shell-file-name shell-command-switch
		   (format "gtags --single-update %s" rel-file-name))))

(defun gtags-update-hook()
  "Update GTAGS file incrementally upon saving a file"
  (when (and gtags-mode gtags-rootdir)
    (gtags-update-current-file gtags-rootdir)))

(add-hook 'after-save-hook 'gtags-update-hook)
