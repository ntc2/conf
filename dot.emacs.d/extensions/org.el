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
    (local-set-key (kbd "C-c C-p") 'outline-backward-same-level)

    (bind-windmove-keys)

    (local-set-key (kbd "C-c C-a") 'align-regexp)))
(add-hook 'org-agenda-mode-hook
          (lambda ()
            (bind-windmove-keys)))

(defun bind-windmove-keys ()
  ;; The `windmove-default-keybindings' binds `S-<direction>' to
  ;; move to window in that direction. By default `org' binds key
  ;; combos to the `org-shift<direction>' functions, e.g. for
  ;; adjusting dates or indentation levels.
  (local-set-key (kbd "C-<left>")  'org-shiftleft)
  (local-set-key (kbd "C-<right>") 'org-shiftright)
  (local-set-key (kbd "C-<up>")    'org-shiftup)
  (local-set-key (kbd "C-<down>")  'org-shiftdown)
  ;; The following doesn't seem to have any effect:
  ;;
  ;; (unbind-key (kbd "S-<left>"))
  ;; (unbind-key (kbd "S-<right>"))
  ;; (unbind-key (kbd "S-<up>"))
  ;; (unbind-key (kbd "S-<down>"))
  ;; (windmove-default-keybindings)
  ;;
  ;; so bind explicitly instead.
  (local-set-key (kbd "S-<left>")  'windmove-left)
  (local-set-key (kbd "S-<right>") 'windmove-right)
  (local-set-key (kbd "S-<up>")    'windmove-up)
  (local-set-key (kbd "S-<down>")  'windmove-down))


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
