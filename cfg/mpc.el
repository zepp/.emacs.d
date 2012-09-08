(require 'mpc)

(setq
 mpc-browser-tags '(Artist Album)
 mpc-songs-format "%3{Track} %25{Title} %-5{Time}")

(defun mpc-toggle-pause ()
  (interactive)
  (cond 
   ((member (cdr (assq 'state (mpc-cmd-status))) '("pause"))
    (mpc-resume))
   ((member (cdr (assq 'state (mpc-cmd-status))) '("play"))
    (mpc-pause))))

(defmacro mpc-sel-window(name)
  `(select-window
    (get-buffer-window
     (get-buffer ,name) t)))

(define-key mpc-mode-map (kbd "s") #'mpc-songs-search)
(define-key mpc-mode-map (kbd "y") #'mpc-playlist-add)
(define-key mpc-mode-map (kbd "k") #'mpc-playlist-delete)
(define-key mpc-mode-map (kbd "M-RET") #'mpc-play-at-point)
(define-key mpc-mode-map (kbd "SPC") #'mpc-toggle-pause)
(define-key mpc-mode-map (kbd "S-SPC") #'mpc-select-toggle)
(define-key mpc-mode-map (kbd "M-SPC") #'mpc-select-extend)
(define-key mpc-mode-map (kbd "n") #'mpc-next)
(define-key mpc-mode-map (kbd "p") #'mpc-prev)
(define-key mpc-mode-map (kbd "l") #'mpc-playlist)
(define-key mpc-mode-map (kbd "M-p") #'mpc-play)

(define-key mpc-mode-map (kbd "RET")
  #'(lambda ()
      (interactive)
      (mpc-select)
      (if (equal "*MPC Artists*" (buffer-name))
	  (mpc-sel-window "*MPC Albums*")
	(mpc-sel-window "*MPC-Songs*"))))

(define-key mpc-mode-map (kbd "a")
  #'(lambda ()
      (interactive)
      (mpc-sel-window "*MPC Albums*")))

(define-key mpc-mode-map (kbd "A")
  #'(lambda ()
      (interactive)
      (mpc-sel-window "*MPC Artists*")))

(define-key mpc-songs-mode-map (kbd "RET") 
  #'mpc-songs-jump-to)
