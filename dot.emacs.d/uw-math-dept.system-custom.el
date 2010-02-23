;; System specific stuff, like setting up modes that I only want on
;; math.wisc.

;; matlab-mode ;;

; This stuff is taken (slightly modified from matlab-mode.el.
(push '("\\.m$" . matlab-mode) auto-mode-alist)
(autoload 'matlab-mode "matlab" "Enter Matlab mode." t)
(autoload 'matlab-shell "matlab" "Interactive Matlab mode." t)
(setq matlab-mode-hook 
      (lambda ()
	(setq matlab-indent-function t)
	(turn-on-auto-fill)
	;; Turn off Matlab desktop
	(setq matlab-shell-command-switches '("-nojvm"))))

;; org-mode
;(setq load-path (cons "~/.emacs.d/org-6.34c/lisp/" load-path))
(add-to-list 'load-path "~/.emacs.d/org-6.34c/lisp/")
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
