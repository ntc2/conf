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
(nc:custom-set-variable flyspell-use-meta-tab nil)
;;
;; but here is a general way:
;;
;;(eval-after-load 'flyspell '(define-key flyspell-mode-map "\M-\t" nil))

;; (add-hook 'font-lock-mode-hook (lambda () (flyspell-mode 1)))

;; Enable `flyspell-mode` or `flyspell-prog-mode` selectively. I
;; separately enable `flyspell-prog-mode` in `./haskell.el` for
;; `haskell-mode`.
;;
;; Default is to enable to `flyspell-mode`.
;;
;; UPDATE: enabling `flyspell-mode' in arbitrary modes as the default
;; case is problematic. E.g. it broke Helm completion in a subtle way,
;; introducing a long delay. It may also have broken other modes that
;; I wasn't aware of ...
(add-hook 'font-lock-mode-hook
  (lambda ()
    ;; To learn what mode you want to add here, do `C-h v major-mode
    ;; RET' in the buffer whose mode you want to know.
    (let ((flyspell-prog-modes
          '(c-mode
            emacs-lisp-mode
            haskell-cabal-mode
            haskell-mode
            lisp-mode
            lisp-interaction-mode
            makefile-gmake-mode
            java-mode
            javascript-mode
            python-mode
            sh-mode))
          (flyspell-modes
           '(conf-mode
             conf-unix-mode
             fundamental-mode
             gfm-mode
             markdown-mode
             org-mode
             rst-mode
             text-mode
             web-mode))
          (no-flyspell-modes
           '(help-mode
             magit-status-mode)))
      (cond ((member major-mode flyspell-prog-modes)
             (flyspell-prog-mode))
            ((member major-mode no-flyspell-modes)
             (flyspell-mode 0))
            ((member major-mode flyspell-modes)
             (flyspell-mode 1))))))
