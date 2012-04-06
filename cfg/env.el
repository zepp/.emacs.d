(if (string= system-type "windows-nt")
    (progn
      (add-to-list 'exec-path "C:/unix/utils/usr/local/wbin")
      (setq
       ;; to do not confuse windows default find and right UNIX find
       find-program "C:\\unix\\utils\\usr\\local\\wbin\\find.exe"
       ;; full name should be specified or one will not be found
       xargs-program "C:\\unix\\utils\\usr\\local\\wbin\\xargs.exe"
       ;; do not want to add all the git stuff to the environment
       vc-git-program "C:\\Program Files (x86)\\Git\\bin\\git.exe"))
  (setq
   browse-url-browser-function 'browse-url-firefox

   ;; disable internal decoder
   base64-internal-decoding-limit 0

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