;; make compilation easier
;; * auto-save before compile
;; * keep compilation window small
;; * auto-close compilation window on success
;; * compile with f10
;;
;; based on
;; http://www.haskell.org/haskellwiki/Haskell-mode#Automatic_unit_testing

(require 'compile)

;; compilation saves the buffer
(setq mode-compile-always-save-buffer-p t)
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
