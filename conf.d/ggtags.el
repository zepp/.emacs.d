(custom-set-variables
 '(ggtags-mode-prefix-key (kbd "M-j"))
 '(ggtags-enable-navigation-keys nil)
 '(ggtags-split-window-function #'split-window-vertically))

(define-key ggtags-mode-prefix-map (kbd "M-t") #'ggtags-find-tag-dwim)
(define-key ggtags-mode-prefix-map (kbd "M-r") #'ggtags-find-reference)

(add-hook 'c-mode-common-hook #'ggtags-mode)
