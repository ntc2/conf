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
(setq mode-compile-always-save-buffer-p t)

;; Compilation window
;;
;; XXX: These functions are kind of annoying: they modify existing
;; windows instead of creating new ones ... better to make them only
;; resize / close if they have to create a *new* window.
;;
;; make compile window 12 lines tall
(setq compilation-window-height 12)
;; from enberg on #emacs
;; if the compilation has a zero exit code, 
;; the windows disappears after two seconds
;; otherwise it stays
(setq compilation-finish-function
      (lambda (buf str)
        (unless (string-match "exited abnormally" str)
          ;;no errors, make the compilation window go away in a few seconds
          (run-at-time
           "2 sec" nil 'delete-windows-on
           (get-buffer-create "*compilation*"))
          (message "No Compilation Errors!"))))

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
