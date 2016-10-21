;; I have to edit bad indented code in some cases and better way is to
;; leave it as is to avoid big VC diffs that hide actual changes, so I
;; put c-mode to exclude list
(add-to-list 'aggressive-indent-excluded-modes 'c-mode)
(add-hook 'after-init-hook #'global-aggressive-indent-mode)
