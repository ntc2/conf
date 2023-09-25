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

;; Disable scroll bars: takes up space.
(scroll-bar-mode 0)

;; Add line numbers to wide windows.
(defun nc/maybe-linum ()
  (if (and (or (derived-mode-p 'prog-mode)
               (derived-mode-p 'text-mode))
           (not (or
                 (derived-mode-p 'org-mode)
                 (derived-mode-p 'magit-mode)))
           (>= (window-width) 70))
      (progn
        (display-line-numbers-mode 1))
    (progn
      (display-line-numbers-mode -1))))
(add-hook 'window-configuration-change-hook #'nc/maybe-linum)

(require 'magit nil t)
(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
