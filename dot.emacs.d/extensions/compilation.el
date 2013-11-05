;; make compilation easier
;; * auto-save before compile
;; * keep compilation window small
;; * auto-close compilation window on success
;; * compile with f10
;; * make error msg visable on jump
;;
;; based on
;; http://www.haskell.org/haskellwiki/Haskell-mode#Automatic_unit_testing

(require 'compile)

;; compilation saves the buffer
(nc:custom-set-variable mode-compile-always-save-buffer-p t)

;; Compilation buffer follows output and stops at first error.
(nc:custom-set-variable compilation-scroll-output 'first-error)
;; Problem: Would like to optionally not stop at warnings, but I'm not sure
;; how to toggle this easily (they're displayed in a different color,
;; so some error matching regexp knows the difference ...).
;;
;; Solution: set 'compilation-skip-threshold'.
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Compilation-Mode.html
;;
;; This affects all the compilation commands, e.g. 'next-error' and
;; 'compilation-next-error'.
(nc:custom-set-variable compilation-skip-threshold 2)

;; Compilation window
;;
;; XXX: These functions are kind of annoying: they modify existing
;; windows instead of creating new ones ... better to make them only
;; resize / close if they have to create a *new* window.
;;
;; make compile window 12 lines tall

;; (setq compilation-window-height 12)

;; Based on code from enberg on #emacs
;;
;; This part does not work in haskell mode's GHCi compilation buffer :P
(setq compilation-finish-functions '(
      (lambda (buf str)
        (unless (string-match "exited abnormally" str)
          ;; No errors, so make the compilation window go away in a
          ;; few seconds. The 'bury-buffer' function could be useful
          ;; here, to push the compilation buffer to the bottom of the
          ;; buffer stack.
          ;;
          ;; If the compilation created a split in the frame, then
          ;; that split will remain. However, that's easy to get rid
          ;; of with 'C-x 1'.
          (run-at-time
           "2 sec" nil 'set-window-buffer (get-buffer-window buf) (other-buffer))
          (message "No Compilation Errors!")))))

;; one-button compilation
(global-set-key [f10] 'compile)

;; Move the error to the *top* of the window after jump (with C-x `)
;;
;; Based on http://lists.gnu.org/archive/html/help-gnu-emacs/2011-09/msg00241.html
;;
;; Subject: Re: Want next-error to move current "hit" to the top of the window -- why doesn't this next-error-hook work?
;;
;; Modified to still work when compile error buffer and src view
;; buffer are in different frames.  When there are more than one
;; compilation style buffers (e.g. from 'M-x grep'), it seems to favor
;; the one in the current frame.
(add-hook 'next-error-hook
  '(lambda ()
     (mapc (lambda (w) (with-selected-window w (recenter 0)))
;; See http://ftp.gnu.org/old-gnu/Manuals/elisp-manual-21-2.8/html_node/elisp_432.html
;; for 'get-buffer-window-list'.
           (get-buffer-window-list
              next-error-last-buffer ; buffer
              nil ; default minibuffer behavior
              t   ; include all windows in all frames
              ))))
