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

;; control-lock

(require 'control-lock)
(control-lock-keys)
