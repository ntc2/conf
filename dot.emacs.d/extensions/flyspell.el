;; Optionally disable `flyspell`.
;;
;; http://www.math.utah.edu/docs/info/elisp_11.html#SEC127 
;;
;; The value specied for a defvar is only used if the variable is
;; undefined when the defvar is evaluated.
(defvar nc:use-flyspell t
  "True if flyspell should be used.  Flyspell causes errors when
  neither of ispell or aspell are installed.")

;(add-hook 'font-lock-mode-hook 'flyspell-prog-mode)
;(add-hook 'text-mode-hook 'flyspell-mode)


;; Enable `flyspell-mode` or `flyspell-prog-mode` selectively. I
;; separately enable `flyspell-prog-mode` in `./haskell.el` for
;; `haskell-mode`.
(add-hook 'font-lock-mode-hook
  (lambda ()
    (when (member major-mode
                  '(text-mode
                    rst-mode
                    latex-mode
                    bibtex-mode
                    fundamental-mode
                    default-generic-mode))
      (flyspell-mode t))
   (when (member major-mode
                 '(emacs-lisp-mode))
     (flyspell-prog-mode))))
