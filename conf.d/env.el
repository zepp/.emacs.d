(if (string= system-type "windows-nt")
    (progn
      (w32-register-hot-key [M-tab])
      (add-to-list 'exec-path "C:/Program Files (x86)/Emacs/EmacsW32/gnuwin32/bin")
      (setenv "PATH" (combine-and-quote-strings exec-path ";")))
  (setq
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

(if (string= (getenv "DESKTOP_SESSION") "stumpwm")
    (setq
     browse-url-browser-function 'browse-url-generic
     browse-url-generic-program "conkeror")
  (setq
   browse-url-browser-function 'browse-url-firefox))
