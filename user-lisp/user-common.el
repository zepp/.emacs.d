;; various auxiliary commands and common functions -*- lexical-binding: t; -*-

(defun u-touch-file (path)
  (unless (file-exists-p path)
    ;; If START is a string, then output that string to the file instead of any
    ;; buffer contents. END is ignored.
    (write-region "" nil path nil 0)))

(defun u-kill-process (&optional buffer)
  "Kills the BUFFER process. BUFFER is the current buffer by
default."

  (interactive
   (list (current-buffer)))

  (let ((proc (get-buffer-process buffer)))
    (if proc
        (interrupt-process proc)
      (user-error "Buffer '%s' has no running process" buffer))))

(defun u-upcase-uuids (&optional buffer)
  "upcases all UUID in the BUFFER"

  (interactive (list (current-buffer)))

  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))

      (let ((uuid-re (string-join
                      '("[0-9a-f]\\{8\\}"
                        "[0-9a-f]\\{4\\}"
                        "[0-9a-f]\\{4\\}"
                        "[0-9a-f]\\{4\\}"
                        "[0-9a-f]\\{12\\}")
                      "-"))
            (case-fold-search nil))
        (while (re-search-forward uuid-re nil t)
          (replace-match (upcase (match-string 0)) t))))))

(defun u-eshell-buf-name (&optional directory)
  "it forms a name of `eshell' buffer that includes name of
DIRECTORY and a host name in case of a remote file
editing. Format is simillar to `project-eshell'."

  (let* ((dir (abbreviate-file-name (or directory default-directory)))
         (dir-name (file-name-nondirectory (directory-file-name dir)))
         (remote-host (file-remote-p dir 'host)))
    (cond
     ((and dir-name remote-host)
      (format "*%s:%s-eshell*" remote-host dir-name))
     ((or dir-name remote-host)
      (format "*%s-eshell*" (or dir-name remote-host)))
     (t "*eshell*"))))

(defun u-rename-eshell-buf(&optional buffer)
  "renames eshell buffer name according to `default-directory'"

  (with-current-buffer (or buffer (current-buffer))

    (unless (derived-mode-p 'eshell-mode)
      (user-error "buffer %s is not a eshell buffer" (buffer-name)))

    (rename-buffer (u-eshell-buf-name) t)))

(defun u-eshell-jump (&optional directory)
  "it starts `eshell' in a current directory or switches buffer to
an existing one"
  (interactive
   (list default-directory))

  ;; do a module autoload otherwise `eshell-buffer-name' is not defined
  (autoload-do-load (symbol-function 'eshell))
  (let* ((eshell-buffer-name (u-eshell-buf-name directory))
         (buf (get-buffer eshell-buffer-name)))
    (cond
     (buf (display-buffer buf '(display-buffer-same-window
                                display-buffer-use-some-window
                                display-buffer-pop-up-window)))
     (directory
      (let ((default-directory directory))
        (eshell)))
     (t (eshell)))))

(defun u-prog-mode-p ()
  (derived-mode-p '(prog-mode sgml-mode nxml-mode)))

(provide 'user-common)
