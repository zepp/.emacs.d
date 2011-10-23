(require 'prefs)

(setq pref-cache-path "~/.emacs.var/pref-cache")

(pref-load-cache)

(pref-append `((root . "~/.emacs.d")
               (src-regexp . "\\.el$")
               (c-style . "gnu")
               (whitespace . 1)
               (indent-tabs . nil)
               (name . "emacs")))

(pref-append `((root . "~/elisp/prefs")
               (src-regexp . "\\.el$")
               (c-style . "gnu")
               (whitespace . 1)
               (indent-tabs . nil)))

(pref-append `(((root . "~/tmp")
                (c-style . "gnu")
                (whitespace . 1)
                (gtags . 1)
                (comp-regexp . gnu)
                (build . "make -k all")
                (clean . "make clean"))
               ((root . "pmplib-0.14")
                (c-style . "linux"))
               ((root . "pmplib-0.14-copy"))
               ((root . "pmplib-0.14-copy-2"))))

