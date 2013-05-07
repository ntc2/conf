;; from
;; http://stackoverflow.com/questions/1781762/enabling-flyspell-mode-gives-an-error
(setq flyspell-issue-welcome-flag nil)

;; twelf
(setq twelf-root "/home/collins/v/twelf-spring-2013.git/bin/twelf/")

;; spire
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-file "/home/collins/v/spire.git/editor/emacs/spire-mode.el")
(require 'spire-mode)
(nc:custom-set-variable spire-command "/home/collins/v/spire.git/spire")
