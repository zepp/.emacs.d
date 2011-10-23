(require 'mpc)

(setq 
 mpc-browser-tags '(Artist Album)
 mpc-songs-format "%3{Track} %25{Title} %-5{Time}")

(define-key mpc-mode-map (kbd "a") 'mpc-playlist-add)
(define-key mpc-mode-map (kbd "d") 'mpc-playlist-delete)
(define-key mpc-mode-map (kbd "l") 'mpc-playlist)
(define-key mpc-mode-map (kbd "p") 'mpc-play-at-point)
(define-key mpc-mode-map (kbd "P") 'mpc-play)
(define-key mpc-mode-map (kbd "u") 'mpc-pause)
(define-key mpc-mode-map (kbd "s") 'mpc-stop)
(define-key mpc-mode-map (kbd "e") 'mpc-songs-search)
(define-key mpc-mode-map (kbd "M-n") 'mpc-next)
(define-key mpc-mode-map (kbd "M-p") 'mpc-prev)
