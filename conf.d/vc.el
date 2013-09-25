(remove-hook 'find-file-hook
             #'vc-find-file-hook)

;; do all the work in same window

(add-to-list 'same-window-buffer-names "*vc-diff*")

(defadvice diff-goto-source
  (around in-same-window (&optional other-file event) activate)
  "show a source file in the current window"
  (let ((display-buffer-function 
         #'(lambda (buf not-this-window)
             (let ((win (selected-window)))
               (set-window-buffer win buffer)
               win))))
    ad-do-it))
