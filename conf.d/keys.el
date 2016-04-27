;;-------------------------------------------------------------------------------
;; global keys definition

;; to make a cursor navigation a little bit easy
(global-set-key (kbd "M-n") #'forward-paragraph)
(global-set-key (kbd "M-p") #'backward-paragraph)

(defun swap-buffers (&optional last)
  "Swaps the current and a last buffers"
  (interactive)
  (let ((buf (current-buffer))
        (frame (selected-frame)))
    (switch-to-buffer
     (if last
         (progn
           (other-buffer buf t frame)
           (bury-buffer buf))
       (last-buffer buf t frame)))))

;; buffer related shortcuts start from C-x 
(global-set-key (kbd "C-x p") #'previous-buffer)
(global-set-key (kbd "C-x n") #'next-buffer)
(global-set-key (kbd "C-x l")
                (lexical-let ((swap-last t))
                  #'(lambda ()
                      "that's a wrapper around the `swap-buffers'
function to keep a state variable"
                      (interactive)
                      (swap-buffers swap-last)
                      (setq swap-last (not swap-last)))))
(global-set-key (kbd "C-x d") #'dired-jump)
(global-set-key (kbd "C-x c") #'shell-jump)
(global-set-key (kbd "C-x C-d") #'dired)
(global-set-key (kbd "C-x M-d") #'dired-other-window)
(global-set-key (kbd "C-x M-f") #'find-file-at-point)
(global-set-key (kbd "C-x M-b") #'switch-to-buffer-other-window)
(global-set-key (kbd "C-x C-n")
                #'(lambda (newname)
                    (interactive
                     (list (read-string "Rename current buffer to: "
                                        (buffer-name (current-buffer)))))
                    (rename-buffer newname)))
(global-set-key (kbd "C-x C-x") #'server-edit)

;; general commands start from C-c
(global-set-key (kbd "C-c C-g") #'rgrep)
(global-set-key (kbd "C-c w") #'browse-url)
;; window management in StumpWM style :)
(global-set-key (kbd "C-c s") #'split-window-horizontally)
(global-set-key (kbd "C-c v") #'split-window-vertically)
(global-set-key (kbd "C-c q") #'delete-other-windows)
(global-set-key (kbd "C-c k") #'delete-window)
(global-set-key (kbd "C-c r") #'iresize-mode)
(global-set-key (kbd "C-c o") #'other-window)

(global-set-key (kbd "S-SPC") #'toggle-input-method)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings 'meta))

(global-set-key (kbd "C-c C-x C-a") 'org-agenda)

