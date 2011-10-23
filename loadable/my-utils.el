(defun emacs-backup-config (output-dir)
  (interactive
   (list (read-directory-name "output directory: "  "~/tmp" nil t)))

  (let* ((path (directory-file-name
                (expand-file-name user-emacs-directory)))
         (dir (car (last (split-string path "[\/]"))))
         (len (- (length path) (length dir)))
         (top-dir (substring path 0 len))
         (archive-name (expand-file-name
                        (format "%s.zip" dir)
                        output-dir))
         (default-directory top-dir))
    (when (file-regular-p archive-name)
      (message (format "removing old archive %s" archive-name))
      (delete-file archive-name))
    (with-temp-buffer
      (let ((files)
            (exit-code
             (call-process shell-file-name 
                           nil (current-buffer) nil
                           shell-command-switch
                           (format "find \"%s\" -type f" path))))
        (if (/= 0 exit-code)
            (progn
              (message (format "find buffer: %s" (buffer-string)))
              (message (format "find error: %i" exit-code)))
          (progn
            (goto-char (point-min))
            (while (not (eobp))
              (let ((path (subseq (buffer-substring-no-properties
                                   (line-beginning-position)
                                   (line-end-position))
                                  len)))
                (when (not (string-match 
                            "\\(tramp$\\)\\|\\(\\.git\\)\\|\\(.*~$\\)\\|\\(.*\\.bak$\\)"
                            path))
                  (setf files (concat files " " path))))
              (forward-line))
            (shell-command (format "zip %s -r %s"
                                   archive-name
                                   files))))))))

(defun kill-region-size()
  (interactive)
  (when (use-region-p)
    (let ((region-size (+ (count-lines (region-end) (region-beginning))
                          (- (region-end) (region-beginning)))))
      (kill-new (format "%s" region-size))
      (message (format "region size: %s" region-size)))
    (deactivate-mark)))

(defun wl-gtube (folder)
  "Write a new draft from Summary."
  (interactive (list (wl-summary-buffer-folder-name)))
  (let ((wl-smtp-posting-port 2525))
    (wl-draft (list (cons 'To "Pavel Sokolov <sokolov@altell.ru>")
                    (cons 'Subject "GTUBE"))
              nil nil 
              "This is the GTUBE, the
  Generic
  Test for
  Unsolicited
  Bulk
  Email

If your spam filter supports it, the GTUBE provides a test by which you
can verify that the filter is installed correctly and is detecting incoming
spam. You can send yourself a test mail containing the following string
of characters (in upper case and with no white spaces and line breaks):

XJS*C4JDBQADN1.NSBN3*2IDNEN*GTUBE-STANDARD-ANTI-UBE-TEST-EMAIL*C.34X

You should send this test mail from an account outside of your network." nil folder)
    (run-hooks 'wl-mail-setup-hook)))