(when (< emacs-major-version 23)
  (setq user-emacs-directory "~/.emacs.d/"))

;only emacs 23.3 and above has string-prefix-p defined
(when (not (and (>= emacs-major-version 23) (>= emacs-minor-version 3)))
  (defun string-prefix-p (str1 str2 &optional ignore-case)
    (eq t (compare-strings str1 nil nil
			   str2 0 (length str1) ignore-case))))

(provide 'config-hacks)
