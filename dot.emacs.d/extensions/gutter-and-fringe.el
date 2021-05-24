;; Perhaps this file is poorly named: it's named by what part of the
;; Emacs UI it affects, not by what it actually does ...

;; Like git-gutter, but that stopped working for me in Emacs 27. The
;; commands are prefixed with `C-x v', e.g. `C-x v =' shows the diff
;; for the current file, focusing on the current hunk.
(global-diff-hl-mode)

;; Make it easy to quickly jump between modified sections of the
;; file. Using some kind of outline/folding mode might be a more
;; general way to achieve this kind of navigation.
(global-set-key (kbd "M-<prior>") 'diff-hl-previous-hunk)
(global-set-key (kbd "M-<next>")  'diff-hl-next-hunk)

;; Use `linum-mode' in all programming modes, except `org-mode'.
(if (version<= "26.0.50" emacs-version )
    ;; In newer versions of emacs use new display-line-numbers
    ;; everywhere, since it doesn't slow down org-mode.
    (global-display-line-numbers-mode)
  (progn
    (add-hook 'prog-mode-hook 'linum-mode)
    (add-hook 'org-mode-hook (lambda () (linum-mode 0)))
    (add-hook 'text-mode-hook 'linum-mode)))

(require 'magit nil t)
(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
