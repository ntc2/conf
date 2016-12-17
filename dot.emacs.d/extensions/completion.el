;;; Completion
;;
;; In `./flyspell.el` we disable `M-TAB` in `flyspell-mode`. By
;; default
;;
;; - `M-TAB`: `complete-symbol`:
;;   https://www.gnu.org/software/emacs/manual/html_node/emacs/Symbol-Completion.html#Symbol-Completion
;;
;; See notes in `./haskell.el` for Haskell specific completion notes.

;; Remove annoying functions from hippie-expand.
;;
;; It might make more sense to leave these functions in, but put them
;; at the end of the list, but I don't remember ever wanting their
;; functionality: if I'm writing the same line over and over in a
;; program than I probably need more abstraction.  Otherwise, I'm
;; probably writing them all at once, in which case 'C-k', 'C-y' should
;; do the job.
(mapc (lambda (x)
        (delete x hippie-expand-try-functions-list))
      '(try-expand-list try-expand-line))

(global-set-key (kbd "M-/") 'hippie-expand)

;; I'm now using `helm' instead of `ido'; see ./helm.el.
(when nil
(require 'ido)
(ido-mode t)
;; Ask if I want to jump to existing visible frame instead of assuming
;; that I do. In XMonad, Emacs always thinks all frames on all visible
;; desktops are visible :P
(nc:custom-set-variable ido-default-file-method 'maybe-frame)
(nc:custom-set-variable ido-default-buffer-method 'maybe-frame)

;; Case-insensitive completion. This is on by default.
(nc:custom-set-variable ido-case-fold t)
)

;; Provides `C-,` and `C-.`, which are also provided by `ido`.
;(icomplete-mode t)


;; For completing e.g. 'h-e-t-f-l' to
;; 'hippie-expand-try-functions-list'
;;
;; Deprecated in Emacs 24:
;;
;(partial-completion-mode t)
;;
;; Instead use:
(nc:custom-set-variable completion-styles '(partial-completion initials))
(nc:custom-set-variable completion-pcm-complete-word-inserts-delimiters t)
