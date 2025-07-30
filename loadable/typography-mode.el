;;; typography-mode.el --- mode to insert various typographic characters

;; Copyright (C) 2025 Pavel Sokolov
;; All rights reserved.

;; Author: Pavel Sokolov <pavel@zeppa.xyz>
;; Maintainer: Pavel Sokolov <pavel@zeppa.xyz>
;; Created: 2025

;; This file is NOT part of GNU Emacs.

;;;###autoload
(define-minor-mode typography-mode
  "Minor mode to insert various typographic characters (quotes, dashes and
so on)"
  ;; The indicator for the mode line.
  :lighter " ¶"
  ;; The minor mode bindings.
  :keymap
  `((,(kbd "M-q") . typography-smart-quote)
    (,(kbd "C-M--") . typography-smart-dash)))

(defcustom typography-quotation-marks-alist
  '((primary . ((start . "«")
                (end . "»")))
    (secondary . ((start . "„")
                  (end . "“")))
    (plain . ((start . "\"")
              (end . "\""))))
  "definition of quotation marks types")

(defcustom typography-dash-length 3
  "length of default dash:
3 - Em dash,
2 - En dash.

Anything else is hyphen.")

(defun typography-build-regexp (type)
  (let* ((marks (alist-get type typography-quotation-marks-alist))
         (start (alist-get 'start marks))
         (end (alist-get 'end marks)))
    (format "^%s[^%s]+%s$" start end end)))

(defun typography-quote (type &optional text)
  (let* ((marks (alist-get type typography-quotation-marks-alist))
         (start (alist-get 'start marks))
         (end (alist-get 'end marks)))
    (format "%s%s%s" start (or text "") end)))

(defun typography-check-quotes (text)
  (cond
   ((string-match (typography-build-regexp 'primary) text)
    'primary)
   ((string-match (typography-build-regexp 'secondary) text)
    'secondary)
   ((string-match (typography-build-regexp 'plain) text)
    'plain)))

;;;###autoload
(defun typography-smart-quote (arg)
  "if region is active then text is wrapped with quotation marks
otherwise ones are just inserted"
  (interactive "p")

  (let ((new-type (if (= arg 1) 'primary 'secondary)))

    (if (use-region-p)
        (let* ((start (region-beginning))
               (end (region-end))
               (text (buffer-substring start end))
               (type (typography-check-quotes text)))
          (when (or
                 (and (eq type 'primary)
                      (eq new-type 'secondary))
                 (and (eq type 'secondary)
                      (eq new-type 'primary))
                 (or (not type) (eq type 'plain)))
            (delete-region start end)
	    (insert (typography-quote
                     new-type
                     (if type
                         (substring text 1 -1)
                       text)))))

      (insert (typography-quote new-type))
      (backward-char 1))))

(defun typography-insert-dash-character (length)
  "inserts em dash, en dash or hyphen character according to `length'"

  (insert-char
   (cond ((= 3 length)
          (char-from-name "EM DASH"))
         ((= 2 length)
          (char-from-name "EN DASH"))
         (t ?-))
   1 t))

;;;###autoload
(defun typography-smart-dash (arg)
  "inserts em dash, en dash or hyphen character according to numeric
prefix argument or replaces current region with dash in case of it is
sequence of hyphens"
  (interactive "p")

  (if (use-region-p)
      (let* ((start (region-beginning))
             (end (region-end))
             (text (buffer-substring start end)))
        (when (string-match "^[-]+$" text)
          (delete-region start end)
	  (typography-insert-dash-character (length text))))

    ;; if `arg' is not specified then default value is 1 but it is not
    ;; suitable
    (typography-insert-dash-character
     (if (numberp current-prefix-arg)
         current-prefix-arg
       typography-dash-length))))

(provide 'typograpy-mode)
