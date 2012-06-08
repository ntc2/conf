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

;; Wish I had iswitchb for xmonad ...
(iswitchb-mode t)
(icomplete-mode t)
;; For completing e.g. 'h-e-t-f-l' to
;; 'hippie-expand-try-functions-list'
(partial-completion-mode t)

