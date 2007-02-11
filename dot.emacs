(custom-set-variables
  ;; custom-set-variables was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 '(case-fold-search t)
 '(current-language-environment "English")
 '(global-font-lock-mode t nil (font-lock))
 '(mouse-wheel-follow-mouse t)
 '(mouse-wheel-mode t nil (mwheel))
 '(ps-line-number t)
 '(save-place t nil (saveplace))
 '(show-paren-mode t nil (paren))
 '(standard-indent 2)
 ; Don't use tabs to indent.  Use spaces instead.
 '(indent-tabs-mode nil)
 '(tab-width 2)
 '(transient-mark-mode t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))
(custom-set-faces
  ;; custom-set-faces was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 )

; Make it darker
(set-foreground-color "grey")
(set-background-color "black")

;;; Some customization from the UW CSL .emacs
(column-number-mode t)
(display-time)

;; If you would like smooth scrolling, uncomment this line
(setq scroll-step 1)

;; For a much better buffer list:
(global-set-key "\C-x\C-b" 'electric-buffer-list)

; Not sure which modes become more decorated?
(setq font-lock-maximum-decoration t)

; A useful looking snippet for setting up custom colors...

;        (font-lock-make-faces t)
;        (setq font-lock-face-attributes
;              '((font-lock-comment-face "Firebrick")
;                (font-lock-string-face "RosyBrown")
;                (font-lock-keyword-face "Purple")
;                (font-lock-function-name-face "Blue")
;                (font-lock-variable-name-face "DarkGoldenrod")
;                (font-lock-type-face "DarkOliveGreen")
;                (font-lock-reference-face "CadetBlue")))

;;; End CSL stuff

;; Make custom modes available

; Add my custom lib dir to the path.
(push "~/.emacs.d" load-path)
; I'll put system (as in the math.wisc.edu vs uoregon.edu) specific
; code here.  This is useful if I want to version control my .emacs,
; because I might not want to load all the same stuff on all systems
; (e.g. no need for matlab-mode on a system without matlab).
(let ((file "~/.emacs.d/system-custom.el"))
  (if (file-exists-p file)
       (load-file file)))
  
