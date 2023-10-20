;; https://200ok.ch/posts/2020-08-22_setting_up_spell_checking_with_multiple_dictionaries.html

(setq ispell-program-name "hunspell")
;; Configure German, Swiss German, and two variants of English.
(setq ispell-dictionary "ru_RU,en_US")
;; ispell-set-spellchecker-params has to be called
;; before ispell-hunspell-add-multi-dic will work
(ispell-set-spellchecker-params)
(ispell-hunspell-add-multi-dic "ru_RU,en_US")
;; For saving words to the personal dictionary, don't infer it from
;; the locale, otherwise it would save to ~/.hunspell_de_DE.
(setq ispell-personal-dictionary "~/.hunspell_personal")

;; The personal dictionary file has to exist, otherwise hunspell will
;; silently not use it.
(unless (file-exists-p ispell-personal-dictionary)
  (write-region "" nil ispell-personal-dictionary nil 0))
