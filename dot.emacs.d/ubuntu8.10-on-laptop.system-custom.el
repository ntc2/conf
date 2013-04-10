;; ;; manual reverse video
;; (set-background-color "black")
;; (set-foreground-color "#e5e5e5")
;; (set-face-foreground 'mode-line "black")
;; ;; in terminal emacs the mode-line background was black in -rv mode,
;; ;; which was bad ...
;; (if (null (window-system))
;;     (set-face-background 'mode-line "yellow"))

;; make org opened links (C-c C-o) open in firefox instead of galeon
(setq browse-url-browser-function 'browse-url-firefox)
;; enforce new window. better to just split out tab of first link
;;opened into new window, then subsequent tabs open in that window if
;;it had focus last.  

;;(setq browse-url-new-window-flag t)

;; proof-general
;;;;;;;;;;;;;;;;

;; See [[file:~/v/org/notes/coq.org *bedrock]] for a way to set the
;; coq-prog-args per project using a .dir-locals.el file.
; (setq coq-prog-args '("-I" "/home/collins/v/cpdt-reading-course.redmine.git/cpdt/src"))

(load-file "~/local/opt/ProofGeneral-4.2/generic/proof-site.el")

(setq proof-three-window-enable t)
(setq proof-electric-terminator-enable t)
(defun nc:del-coq-comments ()
  (interactive)
  (query-replace-regexp "(\\*\\(.\n*\\)*?\\*)\n*" ""))

;; trellys
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; use haskell-mode for .trellys files.  .trellys is close enough .hs
;; that this is useful
(add-to-list 'auto-mode-alist '("\\.trellys\\'" . haskell-mode))

;; twelf
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq twelf-root "/home/collins/v/twelf-spring-2013.git/bin/twelf/")

;; spire
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-file "/home/collins/v/spire.git/editor/emacs/spire-mode.el")
(require 'spire-mode)
(nc:custom-set-variable spire-command "/home/collins/v/spire.git/spire")
