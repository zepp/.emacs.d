;; w3m is web browser

(require 'w3m)
(require 'mime-w3m)
(setq w3m-coding-system 'utf-8
      w3m-file-coding-system 'utf-8
      w3m-file-name-coding-system 'utf-8
      w3m-input-coding-system 'utf-8
      w3m-output-coding-system 'utf-8
      w3m-terminal-coding-system 'utf-8
      w3m-profile-directory (expand-file-name "w3m" my-emacs-var-dir)
      w3m-bookmark-file (expand-file-name "w3m/bookmarks.html" my-emacs-personal-cfg)
      w3m-use-cookies t)
