(require 'flyspell)
(require 'ispell)

(setq

 ispell-highlight-face 'flyspell-incorrect
 ispell-have-new-look t
 ispell-enable-tex-parser t
 flyspell-delay 1
 flyspell-always-use-popup t

 ispell-program-name "aspell"

 ; my dictionary-alist, using for redefinition Russian dictionary
 ispell-dictionary-alist
 '(("english"				; English
    "[a-zA-Z]"
    "[^a-zA-Z]"
    "[']"
    nil
    ("-d" "en")
    ;nil iso-8859-1
    nil utf-8)

   ("russian"				; Russian
    "[АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯабвгдеёжзийклмнопрстуфхцчшщьыъэюя]"
    "[^АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯабвгдеёжзийклмнопрстуфхцчшщьыъэюя]"
    "[-]"
    nil
    ("-C" "-d" "ru")
    nil utf-8)

   (nil					; Default
    "[A-Za-z]"
    "[^A-Za-z]"
    "[']"
    nil
    ("-C")
    ;nil iso-8859-1
    nil utf-8
    ))

 ispell-russian-dictionary "russian"
 ispell-english-dictionary "english"
 flyspell-default-dictionary ispell-english-dictionary
 ispell-dictionary ispell-english-dictionary
 ispell-local-dictionary ispell-russian-dictionary
 ispell-extra-args '("--sug-mode=ultra"))

(defun flyspell-russian ()
  (interactive)
  (flyspell-mode t)
  (ispell-change-dictionary ispell-russian-dictionary)
  (flyspell-buffer)
  (message "Russian dictionary - Spell Checking completed."))

(defun flyspell-english ()
  (interactive)
  (flyspell-mode t)
  (ispell-change-dictionary ispell-english-dictionary)
  (flyspell-buffer)
  (message "English dictionary - Spell Checking completed."))

(define-key flyspell-mode-map (kbd "M-t") #'flyspell-auto-correct-word)
