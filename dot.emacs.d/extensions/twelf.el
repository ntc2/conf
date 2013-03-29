;; http://twelf.org/wiki/Download
;; http://twelf.org/wiki/Twelf_with_Emacs
;;
;; Need to (setq twelf-root ...) to the system specific twelf root in
;; a .dir-locals.el or system specific config file.
(when (boundp 'twelf-root)
  (load (concat twelf-root "emacs/twelf-init.el")))
