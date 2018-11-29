;; See also ./gutter-and-fringe.el for git-gutter config.

;; Edit Git commit messages in diff-mode.
(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG" . diff-mode))

;; As suggested in https://magit.vc/manual/magit/Status-buffer.html,
;; so I can stop typing 'M-x magit-status RET'.
(global-set-key (kbd "C-c m s") 'magit-status)

;; My `git-gutter' doesn't update properly after commits, but this
;; seems to do the trick (but why can't I make this happen
;; automatically ...).
(global-set-key (kbd "C-c m r") 'magit-refresh-all)

;; Highlight word differences in diffs. This is independent from and
;; produces different results than the 'diff-highlight' script I'm
;; using for command line Git, as configured by ~/.gitconfig.
(nc:custom-set-variable magit-diff-refine-hunk 'all)
