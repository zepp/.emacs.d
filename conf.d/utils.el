(defun kill-region-size()
  "calculates the region size and puts one into the kill-ring"

  (interactive)
  (when (use-region-p)
    (let ((region-size (+ (count-lines (region-end) (region-beginning))
                          (- (region-end) (region-beginning)))))
      (kill-new (format "%s" region-size))
      (message (format "region size: %s" region-size)))
    (deactivate-mark)))
