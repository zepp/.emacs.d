(defun shell-jump ()
  "opens the shell in the current directory. Opens new window if
prefix argument is set"

  (interactive)
  (let ((new-shell-buf-name
         (if (eq major-mode 'dired-mode)
             (concat (buffer-name) ":shell")
           (let* ((dir (directory-file-name
                        (if buffer-file-name
                            (file-name-directory buffer-file-name)
                          default-directory))))
             (concat (car (last (split-string dir "/")))
                     ":shell")))))
    (if current-prefix-arg
        (let ((split-height-threshold 0)
              (split-width-threshold nil))
          (shell new-shell-buf-name))
      (let (pop-up-windows
            (same-window-buffer-names
             (cons new-shell-buf-name same-window-buffer-names)))
        (shell new-shell-buf-name)))))

(defun swap-buffers (&optional last)
  "Swaps the current and a last buffers"
  (interactive)
  (let ((buf (current-buffer))
        (frame (selected-frame)))
    (switch-to-buffer
     (if last
         (last-buffer buf t frame)
       (progn
         (other-buffer buf t frame)
         (bury-buffer buf))))))

(defun kill-region-size()
  "calculates the region size and puts one into the kill-ring"

  (interactive)
  (when (use-region-p)
    (let ((region-size (+ (count-lines (region-end) (region-beginning))
                          (- (region-end) (region-beginning)))))
      (kill-new (format "%s" region-size))
      (message (format "region size: %s" region-size)))
    (deactivate-mark)))
