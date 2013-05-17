(require 'iresize)

(define-key iresize-mode-map (kbd "RET") #'iresize-mode)
(define-key iresize-mode-map (kbd "C-f") #'enlarge-window-horizontally)
(define-key iresize-mode-map (kbd "C-b") #'shrink-window-horizontally)
