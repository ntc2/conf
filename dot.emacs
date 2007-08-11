(custom-set-variables
  ;; custom-set-variables was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 '(case-fold-search t)
 '(current-language-environment "English")
 '(global-font-lock-mode t nil (font-lock))
 '(indent-tabs-mode nil)
 '(mouse-wheel-follow-mouse t)
 '(mouse-wheel-mode t nil (mwheel))
 '(ps-black-white-faces (quote ((font-lock-builtin-face "black" nil bold underline) (font-lock-comment-face "gray20" nil italic) (font-lock-constant-face "black" nil bold) (font-lock-function-name-face "black" nil bold) (font-lock-keyword-face "black" nil bold underline) (font-lock-string-face "black" nil italic) (font-lock-type-face "black" nil italic) (font-lock-variable-name-face "black" nil bold italic) (font-lock-warning-face "black" nil bold italic))))
 '(ps-line-number t)
 '(ps-print-color-p (quote black-white))
 '(save-place t nil (saveplace))
 '(show-paren-mode t nil (paren))
 '(standard-indent 2)
 '(tab-width 2)
 '(transient-mark-mode t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))
(custom-set-faces
  ;; custom-set-faces was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 )

; Soft wrap lines in split frames.  Lines in full width frames are
; soft wrapped by default, and lines in split frames are truncated by
; default.
(setq truncate-partial-width-windows nil)

; Always spell check in text mode, and any font-locked mode, which,
; together, is most modes.  Might be better to use M-$ and M-x
; flyspell-region in coding modes ...
(mapc (lambda (hook) (add-hook hook (lambda () (flyspell-mode t))))
      '(text-mode-hook font-lock-mode-hook)) ; I don't really
					     ; understand why I don't need more quotes?

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

; The default history length, at least in sml-run, is apparently 30,
; which is pretty worthless for an interpreter.
(setq history-length 5000)

;; Make emacs shell display ascii color escapes properly.
(ansi-color-for-comint-mode-on)

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

;;; Some customization from
;;; http://www.xsteve.at/prg/emacs/power-user-tips.html

(desktop-load-default) ; From the ``desktop'' docs.

;; save a list of open files in ~/.emacs.desktop
;; save the desktop file automatically if it already exists
(setq desktop-save 'if-exists)
;NC This was causing an undefined var error on startup
;(desktop-save-mode 1)

;; save a bunch of variables to the desktop file
;; for lists specify the len of the maximal saved data also
(setq desktop-globals-to-save
      (append '((extended-command-history . 30)
                (file-name-history        . 100)
                (grep-history             . 30)
                (compile-history          . 30)
                (minibuffer-history       . 50)
                (query-replace-history    . 60)
                (read-expression-history  . 60)
                (regexp-history           . 60)
                (regexp-search-ring       . 20)
                (search-ring              . 20)
                (shell-command-history    . 50)
                tags-file-name
                register-alist)))

(desktop-read) ; From the ``desktop'' docs.


;; Use M-x desktop-save once to save the desktop.  When it exists,
;; Emacs updates it on every exit.  The desktop is saved in the
;; directory where you started emacs, i.e. you have per-project
;; desktops.  If you ever save a desktop in your home dir, then that
;; desktop will be the default in the future when you start emacs in a
;; dir with no desktop.  See the ``desktop'' docs for more info.

;;; End xsteve stuff.

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
  
