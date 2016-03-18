;;; Org mode

;; suggested global bindings from docs
;(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-agenda-files '("~/v/org/"))
(add-hook 'org-mode-hook
  (lambda ()
    ;; turn on soft wrapping mode for org mode, from
    ;; http://osdir.com/ml/emacs-orgmode-gnu/2009-04/msg00618.html
    (setq truncate-lines nil)

    (local-set-key (kbd "C-c C-n") 'outline-forward-same-level)
    (local-set-key (kbd "C-c C-p") 'outline-backward-same-level)))

;; Syntax highlight "src" blocks when they have a language tag, e.g
;;
;; #+begin_src haskell
;; module Main where
;; foo :: Int -> Int
;; foo x = x
;; #+end_src
(nc:custom-set-variable org-src-fontify-natively t)
;; Don't mess with whitespace after editing src blocks using `C-c ''.
(nc:custom-set-variable org-src-preserve-indentation t)
