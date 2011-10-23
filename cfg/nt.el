;;(require 'newsticker)
(autoload 'newsticker-start "newsticker" "Emacs Newsticker" t)
(autoload 'newsticker-show-news "newsticker" "Emacs Newsticker" t)

(setq
;; newsticker-frontend 'newsticker-treeview
 newsticker-frontend 'newsticker-plainview
 newsticker-cache-filename (expand-file-name "newsticker/cache" my-emacs-var-dir)
 newsticker-imagecache-dirname (expand-file-name "newsticker/images" my-emacs-var-dir)
 newsticker-html-renderer 'w3m-region
 newsticker-retrieval-interval 86400
 newsticker-keep-obsolete-items nil
 newsticker-automatically-mark-items-as-old nil
 newsticker-automatically-mark-visited-items-as-old t
 newsticker-show-descriptions-of-new-items nil
 newsticker-url-list-defaults nil)

(remove-hook 'newsticker-buffer-change-hook 'newsticker-w3m-show-inline-images)
(remove-hook 'newsticker-narrow-hook 'newsticker-w3m-show-inline-images)
(remove-hook 'newsticker-new-item-functions 'newsticker-new-item-functions)
