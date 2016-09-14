;; See also ./gutter-and-fringe.el for git-gutter config.

;; Edit Git commit messages in diff-mode.
(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG" . diff-mode))
