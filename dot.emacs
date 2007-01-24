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
 '(standard-indent 4)
 '(transient-mark-mode t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))
(custom-set-faces
  ;; custom-set-faces was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 )

;; Make it darker
(set-foreground-color "grey")
(set-background-color "black")

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
  
