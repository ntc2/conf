(require 'flyspell)

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

;; Tell `flyspell` to not bind `M-TAB`, which is used for completion
;; by default.
;;
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Symbol-Completion.html#Symbol-Completion
;;
;; Flyspell actually provides a special way to do this:
;;
;;(nc:custom-set-variable flyspell-use-meta-tab nil)
;;
;; but here is a general way:
(eval-after-load 'flyspell '(define-key flyspell-mode-map "\M-\t" nil))

;; Enable `flyspell-mode` or `flyspell-prog-mode` selectively. I
;; separately enable `flyspell-prog-mode` in `./haskell.el` for
;; `haskell-mode`.
;;
;; Defaults to `flyspell-mode`.
(add-hook 'font-lock-mode-hook
  (lambda ()
    ;; (when (member major-mode
    ;;               '(bibtex-mode
    ;;                 default-generic-mode
    ;;                 fundamental-mode
    ;;                 latex-mode
    ;;                 org-mode
    ;;                 rst-mode
    ;;                 text-mode))
    ;;   (flyspell-mode t))
   (if (member major-mode
               '(c-mode
                 emacs-lisp-mode
                 haskell-cabal-mode
                 haskell-mode
                 java-mode
                 javascript-mode
                 sh-mode))
       (flyspell-prog-mode)
     (flyspell-mode t))))
