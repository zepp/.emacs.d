(defvar evm-coding-systems-list (make-ring 10))

(ring-insert evm-coding-systems-list 'utf-8)

(defun recode-buffer-dangerous (target-coding-system)
  "* Recode buffer as if it were encoded with `target-coding-system'.
If current buffer is write-protected (`buffer-read-only'),
temporarily toggle read-only flag, recode, then turn it back."
  (interactive "zEnter target coding system: ")
  (labels ((do-recode nil
                      (encode-coding-region (point-min)
                                            (point-max)
                                            buffer-file-coding-system)
                      (decode-coding-region (point-min)
                                            (point-max)
                                            target-coding-system)
                      (set-buffer-file-coding-system target-coding-system)))
    (if buffer-read-only
        (let ((buffer-read-only nil))
          (do-recode)
          (set-buffer-modified-p nil))
      (do-recode))))

(defun recode-buffer-safe (target-coding-system)
  "* Recode buffer as if it were encoded with `target-coding-system'.
If current buffer is write-protected (`buffer-read-only'), do
nothing."
  (interactive "zEnter target coding system: ")
  (unless buffer-read-only
    (encode-coding-region (point-min)
                          (point-max)
                          buffer-file-coding-system)
    (decode-coding-region (point-min)
                          (point-max)
                          target-coding-system)
    (set-buffer-file-coding-system target-coding-system)))

(defun recode-buffer ()
  (interactive)
  (let* ((keys (recent-keys))
         (len (length keys))
         (key1 (if (> len 0) (elt keys (- len 1)) nil))
         (key2 (if (> len 1) (elt keys (- len 2)) nil))
         cs)
    (if (eq key1 key2)
        (setcar evm-coding-systems-list
                (ring-plus1 (car evm-coding-systems-list)
                            (ring-length evm-coding-systems-list)))
      (setcar evm-coding-systems-list 0))
    (set-buffer-multibyte t)
    (recode-buffer-dangerous (aref (cddr evm-coding-systems-list)
                                   (car evm-coding-systems-list)))))
