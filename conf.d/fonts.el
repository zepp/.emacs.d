(if (string= system-type "windows-nt")
    (progn
      (custom-set-faces
       '(default ((t (:height 110 :family "Lucida Console")))))
      (add-to-list 'default-frame-alist '(font . "Lucida Console-11")))
  (progn
    (custom-set-faces
     '(default ((t (:height 110 :family "Inconsolata")))))
    (add-to-list 'default-frame-alist '(font . "Inconsolata-11"))))
