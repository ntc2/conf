(set-face-foreground 'mode-line "black")
;; in terminal emacs the mode-line background was black in -rv mode,
;; which was bad ...
(if (null? (window-system))
    (set-face-background 'mode-line "yellow"))
