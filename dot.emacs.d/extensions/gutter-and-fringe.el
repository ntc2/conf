;; Perhaps this file is poorly named: it's named by what part of the
;; Emacs UI it affects, not by what it actually does ...

;; Enable linum-mode and git-gutter-mode minor modes in all non-org
;; major modes. Using the same hack as in ./flyspell.el -- i.e. piggy
;; backing on font lock hook -- to do run this in essentially all
;; major modes that correspond to text editing.
;;
;; It might be OK to use git-gutter-mode with org-mode; I haven't
;; tested, but folding surely can't make sense.
(when (require 'git-gutter-fringe nil t)
  (add-hook 'font-lock-mode-hook
            (lambda ()
              (when (not (member major-mode '(org-mode)))
                (git-gutter-mode 1)
                (linum-mode 1)))))

;; Make git-gutter work with magit. By default, when committing from
;; magit-status, git-gutter doesn't realize a commit has been made.
;;
;; See
;; http://stackoverflow.com/questions/23344540/emacs-update-git-gutter-annotations-when-staging-or-unstaging-changes-in-magit.
(when (and (require 'git-gutter-fringe nil t)
           (require 'magit nil t))
  (add-hook 'magit-post-refresh-hook
            #'git-gutter:update-all-windows))
