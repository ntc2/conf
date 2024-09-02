;; See also ./gutter-and-fringe.el for git-gutter config.

;; Edit Git commit messages in diff-mode.
(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG" . diff-mode))

(use-package which-key
  :config
  (which-key-add-key-based-replacements
    "C-c m" "magit"))

;; I want the default before for wide monitors, but want to fill vertically for
;; small windows.  The built-in `magit-display-buffer-full-column-most-v1' only
;; handles the second case, so instead we reuse the default "traditional"
;; display mode, and then just force it to fill vertically. Note that the
;; `magit-display-buffer-function' needs to return the window it displayed to.
(defun nc/display-buffer-fullcolumn (buffer)
  (let ((window (magit-display-buffer-traditional buffer)))
    (delete-other-windows-vertically window)
    window))

(use-package magit
  :custom
  (magit-display-buffer-function 'nc/display-buffer-fullcolumn)
  :config
  ;; Enable `git absorb' in menu. Need to install `git absorb' separately.
  (transient-replace-suffix 'magit-commit 'magit-commit-autofixup
  '("x" "Absorb changes" magit-commit-absorb)))

;; As suggested in https://magit.vc/manual/magit/Status-buffer.html,
;; so I can stop typing 'M-x magit-status RET'.
(global-set-key (kbd "C-c m s") 'magit-status)
(global-set-key (kbd "<f5>") 'magit-status)

;; Popup the magit quick menu. Many options available here, e.g. press `d' to
;; see diff for hunk at point.
(global-set-key (kbd "C-c m m") 'magit-file-dispatch)

;; My `git-gutter' doesn't update properly after commits, but this
;; seems to do the trick (but why can't I make this happen
;; automatically ...).
(global-set-key (kbd "C-c m r") 'magit-refresh-all)

;; Popup a buffer showing the hunk and point and ask to revert
;; it. Answering "no" or hitting `C-g' results in the popup buffer
;; getting buried and the hunk not getting reverted, so it's also a
;; way to see hunk diffs without changing to Magit status mode.
(global-set-key (kbd "C-c m h") 'diff-hl-show-hunk)

;; Highlight word differences in diffs. This is independent from and
;; produces different results than the 'diff-highlight' script I'm
;; using for command line Git, as configured by ~/.gitconfig.
(nc:custom-set-variable magit-diff-refine-hunk 'all)

;; Wrap long lines in Magit diffs, instead of truncating them (the
;; default).
;;
;; There is also `word-wrap' (off by default), which makes wrapping
;; respect word boundaries.
(defun wrap-lines ()
  (setq truncate-lines nil))
(add-hook 'magit-status-mode-hook 'wrap-lines)
(add-hook 'magit-diff-mode-hook 'wrap-lines)
