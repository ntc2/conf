;;; Org mode

;; Enable the #+BEGIN_* shortcuts.
;;
;; https://orgmode.org/manual/Structure-Templates.html#Structure-Templates
(require 'org-tempo)

;; suggested global bindings from docs
;;
;;
;;(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
;;
;; Default files for "C-c a".
;;
;;(setq org-agenda-files '("~/v/org/"))

;; Org todo keywords
;;
;; For todo items, the the priority is INPROGRESS > NEXT > TODO.
;;
;; Use separate groups so that they can be cycled separately by using
;; `C-c C-t'. The `C-<left>' and `C-<right>' cycle through all
;; sets. Docs:
;; http://orgmode.org/manual/Multiple-sets-in-one-file.html#Multiple-sets-in-one-file
;;
;; Use single-letter shortcuts (e.g. "t" in "TODO(t)") for fast access
;; to TODO state, e.g. with `C-c C-t t' to set to "TODO". Docs:
;; http://orgmode.org/manual/Fast-access-to-TODO-states.html#Fast-access-to-TODO-states
(nc:custom-set-variable org-todo-keywords
                        '((sequence "TODO(t)" "NEXT(n)" "INPROGRESS(p)" "|" "DONE(d)")
                          (sequence "|" "MAYBE(m)" "WAITING(w)" "INREVIEW(r)" "CANCELLED(c)")))

(defun nc/org/start-work-day ()
  "Insert subheading titled with today's date and clock in under it."
  (interactive)
  (org-insert-subheading "")
  (insert (format-time-string "[%Y-%m-%d %a]"))
  (org-clock-in)
  ;; Start a new list item inside the clocked-in subheading.
  (execute-kbd-macro (kbd "<down> <tab> <home> C-o - SPC")))

(add-hook 'org-mode-hook
  (lambda ()
    ;; Don't indent drawers (e.g the LOGBOOK for clocking)
    (setq org-adapt-indentation nil)

    ;; turn on soft wrapping mode for org mode, from
    ;; http://osdir.com/ml/emacs-orgmode-gnu/2009-04/msg00618.html
    (setq truncate-lines nil)

    (local-set-key (kbd "C-c C-n") 'outline-forward-same-level)
    (local-set-key (kbd "C-c C-p") 'outline-backward-same-level)

    (bind-windmove-keys)

    (local-set-key (kbd "C-c C-a") 'align-regexp)

    ;; By default, `org-mode-map' remaps `kill-line' to `org-kill-line', which
    ;; in turn inherits the default behavior of `visual-line-mode's kill line
    ;; function, which only kills to the end of the visual/logical line. I want
    ;; to kill to the end of the physical line, and i've separately overridden
    ;; `C-k' in `visual-line-mode', so here I just need to remove the remapping.
    (define-key org-mode-map [remap kill-line] nil)

    (local-set-key (kbd "<f6>") 'nc/org/start-work-day)

    ;; Search Org headings using Helm.
    (local-set-key (kbd "C-c o s") 'helm-org-in-buffer-headings)))
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
;; Start org mode buffers collapsed. I swear this used to happen
;; automatically ...
(nc:custom-set-variable org-startup-folded t)
