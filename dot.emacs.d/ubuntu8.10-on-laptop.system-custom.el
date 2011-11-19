;; manual reverse video
(set-background-color "black")
(set-foreground-color "#e5e5e5")
(set-face-foreground 'mode-line "black")
;; in terminal emacs the mode-line background was black in -rv mode,
;; which was bad ...
(if (null (window-system))
    (set-face-background 'mode-line "yellow"))

;; make org opened links (C-c C-o) open in firefox instead of galeon
(setq browse-url-browser-function 'browse-url-firefox)
;; enforce new window. better to just split out tab of first link
;;opened into new window, then subsequent tabs open in that window if
;;it had focus last.  

;;(setq browse-url-new-window-flag t)

;; proof-general
;;;;;;;;;;;;;;;;
(setq coq-prog-args '("-I" "/home/collins/v/cpdt-reading-course.redmine.git/cpdt/src"))
;; "~" doesn't work here, but does if passed on command line, or used in a .v file with LoadPath ???
;(setq coq-prog-args '("-I" "~/v/cpdt-reading-course.redmine.git/cpdt/src"))
(setq proof-three-window-enable t)
(setq proof-electric-terminator-enable t)
(defun nc:del-coq-comments ()
  (interactive)
  (query-replace-regexp "(\\*\\(.\n*\\)*?\\*)\n*" ""))

;; use haskell-mode for .trellys files.  .trellys is close enough .hs
;; that this is useful
(add-to-list 'auto-mode-alist '("\\.trellys\\'" . haskell-mode))
