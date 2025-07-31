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
    (,(kbd "\"") . typography-smart-quote)
    (,(kbd "C-M--") . typography-smart-dash)))

(defgroup typography nil "Main group")

(defun typography-quotes (symbol start-character end-character)
  "utility function to build `typography-quotation-marks-alist' entry"
  `(,symbol . ((start . ,start-character)
               (end . ,end-character))))

(defcustom typography-quotation-marks-alist
  (list (typography-quotes 'primary ?« ?»)
        (typography-quotes 'secondary ?\„ ?\“)
        (typography-quotes 'neutral ?\" ?\"))

  "Alist that keeps definition of quotation marks types: primary, secondary
and neutral. It is utilized by `typography-smart-quote' command.

`typography-quotes' helper function builds entry for this alist."

  :type '(sexp)
  :group 'typography)

(defcustom typography-dash-length 3
  "Default dash length for `typography-smart-dash' command if prefix
argument is not specified.

Description:
- 3 is em dash,
- 2 is en dash,
- anything else is short dash."

  :type '(natnum)
  :group 'typography)

(defun typography-build-regexp (type)
  (let* ((marks (alist-get type typography-quotation-marks-alist))
         (start (alist-get 'start marks))
         (end (alist-get 'end marks)))
    (format "^%c[^%c]+%c$" start end end)))

(defun typography-quote (type &optional text)
  (let* ((marks (alist-get type typography-quotation-marks-alist))
         (start (alist-get 'start marks))
         (end (alist-get 'end marks)))
    (format "%c%s%c" start (or text "") end)))

(defun typography-check-quotes (text)
  (cond
   ((string-match (typography-build-regexp 'primary) text)
    'primary)
   ((string-match (typography-build-regexp 'secondary) text)
    'secondary)
   ((string-match (typography-build-regexp 'neutral) text)
    'neutral)))

;;;###autoload
(defun typography-smart-quote (arg)
  "if region is active then text is wrapped with quotation marks
otherwise ones are just inserted. Numeric prefix argument
specifies quotation marks type: 1 stands for primary, 2 stands
for secondary and anything else for neutral quotes."

  (interactive "p")

  (let ((new-type (cond
                   ((= arg 1) 'primary)
                   ((= arg 2) 'secondary)
                   (t 'neutral))))

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
                 (or (not type) (eq type 'neutral)))
            (delete-region start end)
	    (insert (typography-quote
                     new-type
                     (if type
                         (substring text 1 -1)
                       text)))))

      (insert (typography-quote new-type))
      (backward-char 1))))

(defun typography-insert-dash-character (length)
  "inserts em dash, en dash or short dash character according to `length'"

  (insert-char
   (cond ((= 3 length)
          (char-from-name "EM DASH"))
         ((= 2 length)
          (char-from-name "EN DASH"))
         (t ?-))
   1 t))

;;;###autoload
(defun typography-smart-dash (arg)
  "inserts dash character according to prefix argument or replaces
current region with dash in case of it is sequence of
hyphens.

`typography-dash-length' specifies default argument value in case if it
is not specified."

  (interactive "P")

  (if (use-region-p)
      (let* ((start (region-beginning))
             (end (region-end))
             (text (buffer-substring start end)))
        (when (string-match "^[-]+$" text)
          (delete-region start end)
	  (typography-insert-dash-character (length text))))

    (typography-insert-dash-character
     (if (integerp arg)
         arg
       typography-dash-length))))

(provide 'typograpy-mode)
