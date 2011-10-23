;;; typography-mode.el --- mode to insert various typographic characters -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pavel Sokolov
;; All rights reserved.

;; Author: Pavel Sokolov <pavel.zepp@gmail.com>
;; Maintainer: Pavel Sokolov <pavel.zepp@gmail.com>
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

(defun typography-quotes (method primary secondary)
  "utility function to build `typography-quotation-marks-alist' entry"

  `(,method . ,`((primary . ,primary)
                 (secondary . ,secondary))))

(defcustom typography-quotation-marks-alist
  (list (typography-quotes 'english-computer
                           (cons ?\‘ ?\’) (cons ?\“ ?\”))
        (typography-quotes 'russian-computer
                           (cons ?« ?») (cons ?\„ ?\“))
        (typography-quotes 'neutral-computer
                           (cons ?\" ?\") (cons ?\' ?\')))

  "Alist that keeps definition of quotation marks types: primary, secondary
and neutral. It is utilized by `typography-smart-quotes' command.

`typography-quotes' helper function builds entry for this alist."

  :type '(sexp)
  :group 'typography)

(defcustom typography-dash-alist '((english-computer . 2)
                                   (russian-computer . 3)
                                   (neutral-computer . 1))
  "Alist that specifies dash length for `typography-smart-dash'
command if prefix argument is not specified.

Description:
- 3 is em dash,
- 2 is en dash,
- anything else is short dash."

  :type '(sexp)
  :group 'typography)

(defun typography-current-im ()
  "returns current input method as a symbol"

  (if current-input-method
      (intern current-input-method)
    'english-computer))

(defun typography-quotes-regexp (type alist)
  (let* ((marks (alist-get type alist))
         (opening (car marks))
         (closing (cdr marks)))
    (format "^%c[^%c]+%c$" opening closing closing)))

(defun typography-check-quotes-type (text alist)
  (cond
   ((string-match (typography-quotes-regexp 'primary alist) text)
    'primary)
   ((string-match (typography-quotes-regexp 'secondary alist) text)
    'secondary)))

(defun typography-check-quotes (text)
  (seq-some #'(lambda (im-alist)
                (let* ((im (car im-alist))
                       (alist (cdr im-alist))
                       (type (typography-check-quotes-type text alist)))
                  (when type
                    (cons im type))))
            typography-quotation-marks-alist))

(defun typograpy-method-marks (&optional method)
  (alist-get (or method (typography-current-im))
             typography-quotation-marks-alist))

(defun typography-insert-quoted (text type)
  "inserts quoted text using quotes style of the current input
method"

  (insert
   (let* ((marks (alist-get type (typograpy-method-marks)))
          (opening (car marks))
          (closing (cdr marks)))
     (format "%c%s%c" opening text closing))))

;;;###autoload
(defun typography-smart-quotes (arg)
  "if region is active then text is wrapped with quotation marks
otherwise ones are just inserted. Numeric prefix argument
specifies quotation marks type: 1 stands for primary, 2 stands
for secondary."

  (interactive "P")

  (let ((arg-type (cond
                   ((equal arg 1) 'primary)
                   ((equal arg 2) 'secondary))))

    (if (use-region-p)
        (let* ((current-im (typography-current-im))
               (start (region-beginning))
               (end (region-end))
               (text (buffer-substring start end))
               (quotes-style (typography-check-quotes text))
               (im (car quotes-style))
               (type (cdr quotes-style)))
          (cond
           ((and quotes-style (eq im current-im)
                 arg-type (not (eq type arg-type)))
            (delete-region start end)
            (typography-insert-quoted
             (substring text 1 -1)
             arg-type))

           ((and quotes-style (not (eq im current-im)))
            (delete-region start end)
            (typography-insert-quoted
             (substring text 1 -1)
             (or arg-type type)))

           ((not quotes-style)
            (delete-region start end)
            (typography-insert-quoted
             text
             (or arg-type 'primary)))

           (t (user-error
                   "typography: pointless conversion from %s/%s quotes to the same \
style" im type))))

      (typography-insert-quoted "" (or arg-type 'primary))
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
       (alist-get (typography-current-im) typography-dash-alist)))))

(provide 'typograpy-mode)
