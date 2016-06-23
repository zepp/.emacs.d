(add-hook 'c-mode-common-hook
          #'(lambda ()
              (auto-fill-mode 1)
              (c-toggle-hungry-state 1)
              (c-toggle-auto-newline -1)))
