;;; Haskell
;;;
;;; http://www.haskell.org/haskellwiki/Haskell_mode_for_Emacs
;;; http://projects.haskell.org/haskellmode-emacs/

(add-hook 'haskell-mode-hook
          ;;see http://www.haskell.org/haskellwiki/Haskell-mode
          ;;         #inf-haskell.el:_the_best_thing_since_the_breadknife
          (lambda () (require 'inf-haskell)))

;; Now managed by el-get. I'm making sure el-get is loaded before this
;; file, but I think there's a blessed way to config packages
;; installed by el-get.

;(load "~/local/opt/haskellmode-emacs/haskell-site-file" t)

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

;;; indentation modes
;;;
;;; Debian systems with haskell-mode installed add a hook enabling
;;; fancy indentation in /etc/emacs/site-start.d/50haskell-mode.el, so
;;; we remove that hook here before adding our preferred hook.


;; the three indentation modes are mutually exclusive ... and i don't
;; know where the differences are documented :PA

;; this one is supposed to be the most fancy.
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(remove-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; this one has the "insert the function name and a space" behavior
;; when tabbing under a function def, which the "fancy" one does not.
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;; this one just tabs to the beginning of the next token on the
;; previous line, like text-mode.
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

;; Avoid custom prompt trouble.  Instead, I factored out the prompt
;; into ~/.ghc/ghci-prompt.conf, which is loaded by `nc:ghci`.  Kind
;; of annoying but e.g. the below does not work, since the
;; -ignore-dot-ghci causes ghci to ignore the subsequent -ghci-script
;; (bug?).  If I ever want to avoid the `nc:ghci` hack, I might look
;; at the variables:
;;
;;   inferior-haskell-send-command
;;   inferior-haskell-wait-for-prompt
;;   comint-prompt-regexp
;;
;; defined by inf-haskell.el and comint mode.

;;(setq haskell-program-name "ghci -ignore-dot-ghci -ghci-script ~/.ghc/non-prompt.ghci")

;; Indent 2 spaces
(setq haskell-indent-offset 2)
