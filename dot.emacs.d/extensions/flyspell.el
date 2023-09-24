(use-package flyspell
  :bind
  ;; This is bound to `C-;' already, but since I upgraded to Ubuntu
  ;; 22.04 something prevents that key from making it to Emacs.
  (("C-:" . flyspell-auto-correct-previous-word))
  :custom
  ;; Tell `flyspell` to not bind `M-TAB`, which is used for completion
  ;; by default.
  ;;
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Symbol-Completion.html#Symbol-Completion
  (flyspell-use-meta-tab nil))

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
             latex-mode
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
