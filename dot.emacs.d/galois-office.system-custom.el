(desktop-save-mode 1)
(setq history-length 250)
(add-to-list 'desktop-globals-to-save 'file-name-history)

;; Use my modified haskell-mode until my patch is accepted.
(add-to-list 'load-path "~/v/haskell-mode.fork.git/")
;; Use my modified intero.
;;
;; Use this to override what version of the `intero' executable is
;; used by the Intero Emacs mode when a the released version of the
;; Intero Emacs mode uses a too old `intero' executable.
;;
;; (add-to-list 'load-path "~/v/intero.fork.git/elisp/")
