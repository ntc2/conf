;; http://www.math.utah.edu/docs/info/elisp_11.html#SEC127 
;;
;; The value specied for a defvar is only used if the variable is
;; undefined when the defvar is evaluated.
(defvar nc:use-flyspell t
  "True if flyspell should be used.  Flyspell causes errors when
  neither of ispell or aspell are installed.")

;(add-hook 'font-lock-mode-hook 'flyspell-prog-mode)
;(add-hook 'text-mode-hook 'flyspell-mode)

; Turn on flyspell-prog-mode in all modes except ..., which should use
; full flyspell-mode.
;
; Might be better to do the opposite: specify which modes should use
; flyspell-prog-mode, and default to plain flyspell. The problem is
; that flyspell-prog-mode is confusing when it doesn't work.
(when nc:use-flyspell
  (add-hook 'font-lock-mode-hook
            (lambda ()
              (if (member major-mode '(text-mode rst-mode latex-mode bibtex-mode
                                       fundamental-mode default-generic-mode))
                  (flyspell-mode t)
                (flyspell-prog-mode)))))
