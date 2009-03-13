;; manual reverse video
(set-background-color "black")
(set-foreground-color "#e5e5e5")
(set-face-foreground 'mode-line "black")
;; in terminal emacs the mode-line background was black in -rv mode,
;; which was bad ...
(if (null (window-system))
    (set-face-background 'mode-line "yellow"))
