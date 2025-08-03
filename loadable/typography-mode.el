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
  `((,(kbd "M-q") . typography-smart-quotes)
    (,(kbd "\"") . typography-smart-quotes)
    (,(kbd "C-M--") . typography-smart-dash)))

(defgroup typography nil "Main group")

(defun typography-quotes (symbol opening-character closing-character)
  "utility function to build `typography-quotation-marks-alist' entry"
  `(,symbol . ((opening . ,opening-character)
               (closing . ,closing-character))))

(defcustom typography-quotation-marks-alist
  (list (typography-quotes 'primary ?« ?»)
        (typography-quotes 'secondary ?\„ ?\“)
        (typography-quotes 'neutral ?\" ?\"))

  "Alist that keeps definition of quotation marks types: primary, secondary
and neutral. It is utilized by `typography-smart-quotes' command.

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

(defun typography-quotes-regexp (type)
  (let* ((marks (alist-get type typography-quotation-marks-alist))
         (opening (alist-get 'opening marks))
         (closing (alist-get 'closing marks)))
    (format "^%c[^%c]+%c$" opening closing closing)))

(defun typography-check-quotes (text)
  (cond
   ((string-match (typography-quotes-regexp 'primary) text)
    'primary)
   ((string-match (typography-quotes-regexp 'secondary) text)
    'secondary)
   ((string-match (typography-quotes-regexp 'neutral) text)
    'neutral)))

(defun typography-insert-quoted (text type)
  (insert
   (let* ((marks (alist-get type typography-quotation-marks-alist))
          (opening (alist-get 'opening marks))
          (closing (alist-get 'closing marks)))
     (format "%c%s%c" opening text closing))))

;;;###autoload
(defun typography-smart-quotes (arg)
  "if region is active then text is wrapped with quotation marks
otherwise ones are just inserted. Numeric prefix argument
specifies quotation marks type: 1 stands for primary, 2 stands
for secondary and anything else for neutral quotes."

  (interactive "p")

  (let ((arg-type (cond
                   ((= arg 1) 'primary)
                   ((= arg 2) 'secondary)
                   (t 'neutral))))

    (if (use-region-p)
        (let* ((start (region-beginning))
               (end (region-end))
               (text (buffer-substring start end))
               (type (typography-check-quotes text)))
          (cond
           ((or
             (and (eq type 'primary)
                  (eq arg-type 'secondary))
             (and (eq type 'secondary)
                  (eq arg-type 'primary))
             (and (eq type 'neutral)
                  (not (eq arg-type 'neutral))))
            (delete-region start end)
            (typography-insert-quoted
             (substring text 1 -1)
             arg-type))

           ((not type)
            (delete-region start end)
            (typography-insert-quoted
             text
             arg-type))

           (t (user-error
               "typography: pointless conversion from %s to %s quotation marks"
               type arg-type))))

      (typography-insert-quoted "" arg-type)
      (backward-char 1))))

(defun typography-insert-dash (length)
  "inserts em dash, en dash or short dash character according to `length'"

  (insert-char
   (cond ((= 3 length)
          (char-from-name "EM DASH"))
         ((= 2 length)
          (char-from-name "EN DASH"))
         ((= 1 length) ?-)
         (t (user-error "typography: incorrect dash length")))
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
        (if (not (string-match "^[-]+$" text))
            (user-error "typography: region is not sequence of hyphens")
          (delete-region start end)
	  (typography-insert-dash (length text))))

    (typography-insert-dash
     (if (integerp arg)
         arg
       typography-dash-length))))

(provide 'typograpy-mode)
