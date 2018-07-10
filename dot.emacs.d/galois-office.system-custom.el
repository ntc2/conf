(desktop-save-mode 1)
(setq history-length 250)
(add-to-list 'desktop-globals-to-save 'file-name-history)

;; Use my modified haskell-mode until my patch is accepted.
(add-to-list 'load-path "~/v/haskell-mode.fork.git/")
(add-to-list 'load-path "~/v/intero.fork.git/elisp/")
