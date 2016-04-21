(ido-mode 0)
(require 'helm-config)
(helm-mode 1)
(helm-autoresize-mode 1)

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(define-key shell-mode-map (kbd "C-c C-l") 'helm-comint-input-ring)
(define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)

(setf helm-split-window-in-side-p t       ; open helm buffer inside current window, not occupy whole other window
      helm-autoresize-max-height 20
      helm-move-to-line-cycle-in-source t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-file-name-history-use-recentf t
      helm-buffers-fuzzy-matching t 
      helm-recentf-fuzzy-match t
      helm-M-x-fuzzy-match t)

(mapcar #'(lambda (entry)
            (add-to-list 'helm-boring-buffer-regexp-list entry))
        ignored-buffer-list)

(custom-set-variables
 '(helm-gtags-prefix-key "\C-t")
 '(helm-gtags-suggested-key-mapping nil)
 '(helm-gtags-use-input-at-cursor t)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-auto-update t)
 '(helm-gtags-update-interval-second nil)
 '(helm-gtags-fuzzy-match t))

(eval-after-load 'helm-gtags
  '(progn 
     (define-key helm-gtags-mode-map (kbd "C-t h") #'helm-gtags-display-browser)
     (define-key helm-gtags-mode-map (kbd "C-t P") #'helm-gtags-find-files)
     (define-key helm-gtags-mode-map (kbd "C-t f") #'helm-gtags-parse-file)
     (define-key helm-gtags-mode-map (kbd "C-t g") #'helm-gtags-find-pattern)
     (define-key helm-gtags-mode-map (kbd "C-t s") #'helm-gtags-find-symbol)
     (define-key helm-gtags-mode-map (kbd "C-t r") #'helm-gtags-find-rtag)
     (define-key helm-gtags-mode-map (kbd "C-t t") #'helm-gtags-find-tag)
     (define-key helm-gtags-mode-map (kbd "C-t d") #'helm-gtags-find-tag)
     (define-key helm-gtags-mode-map (kbd "C-t C-t") #'helm-gtags-pop-stack)
     (define-key helm-gtags-mode-map (kbd "M-*") #'helm-gtags-pop-stack)
     (add-hook 'c-mode-common-hook #'helm-gtags-mode)))
