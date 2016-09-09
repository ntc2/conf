;; Perhaps this file is poorly named: it's named by what part of the
;; Emacs UI it affects, not by what it actually does ...

(require 'git-gutter-fringe)

;; Enable linum-mode and git-gutter-mode minor modes in all non-org
;; major modes. Using the same hack as in ./flyspell.el -- i.e. piggy
;; backing on font lock hook -- to do run this in essentially all
;; major modes that correspond to text editing.
;;
;; It might be OK to use git-gutter-mode with org-mode; I haven't
;; tested, but folding surely can't make sense.
(add-hook 'font-lock-mode-hook
          (lambda ()
            (when (not (member major-mode '(org-mode)))
              (git-gutter-mode 1)
              (linum-mode 1))))
