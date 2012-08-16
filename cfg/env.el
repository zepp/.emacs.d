(if (string= system-type "windows-nt")
    (progn
      (add-to-list 'exec-path "C:/Program Files (x86)/Emacs/EmacsW32/gnuwin32/bin")
      (setenv "PATH" (apply #'concat
			    (maplist #'(lambda (e)
					 (let ((p (convert-standard-filename (car e))))
					   (if (cdr e)
					       (concat p ";")
					     p)))
				     exec-path))))
  (setq
   browse-url-browser-function 'browse-url-firefox

   ;; disable internal decoder
   base64-internal-decoding-limit 0
   base64-internal-encoding-limit 0

   quoted-printable-internal-encoding-limit 0
   quoted-printable-internal-decoding-limit 0

   ;; using metamail
   ;; base64-external-encode '("mimencode")
   ;; base64-external-decode '("mimencode" "-u")
   ;; quoted-printable-external-encoder '("mimencode" "-q")
   ;; quoted-printable-external-decoder '("mimencode" "-q" "-u")

   ;; using sharutils
   base64-external-encode '("uuencode" "-m")
   base64-external-decode '("uudecode" "-m")
   quoted-printable-external-encoder '("uuencode")
   quoted-printable-external-decoder '("uudecode")))
