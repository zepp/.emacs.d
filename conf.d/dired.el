(setq dired-bind-jump nil
      dired-recursive-deletes 'always
      dired-deletion-confirmer #'y-or-n-p)
(require 'dired-x)

(defadvice dired-do-shell-command
    (around split-fashion (command &optional arg file-list)
            activate)
  "Controls the fashion of window splitting. Splits window
vertically."
  (let ((split-height-threshold 0)
        (split-width-threshold nil))
    ad-do-it))

(defadvice dired-do-async-shell-command
    (around split-fashion (command &optional arg file-list)
            activate)
  "Controls the fashion of window splitting. Splits window
vertically."
  (let ((split-height-threshold 0)
        (split-width-threshold nil))
    ad-do-it))

(define-key dired-mode-map (kbd "M-n") #'dired-next-line)
(define-key dired-mode-map (kbd "M-p") #'dired-previous-line)
(define-key dired-mode-map (kbd "c") #'dired-do-copy)
(define-key dired-mode-map (kbd "d") #'dired-do-delete)
(define-key dired-mode-map (kbd "r") #'dired-do-rename)
(define-key dired-mode-map (kbd "M-d") #'dired-flag-file-deletion)

(put 'dired-find-alternate-file 'disabled nil)

;; -l is mandatory
;; -G omit the group name
;; -h human-readable size
(setq dired-listing-switches "-alGh")
