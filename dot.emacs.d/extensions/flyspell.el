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
(when nc:use-flyspell
  (add-hook 'font-lock-mode-hook
            (lambda ()
              (if (member major-mode '(text-mode rst-mode latex-mode fundamental-mode))
                  (flyspell-mode t)
                (flyspell-prog-mode)))))
