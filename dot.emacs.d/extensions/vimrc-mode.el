;; From
;; http://stackoverflow.com/questions/4236808/syntax-highlight-a-vimrc-file-in-emacs,
;; who got it from
;; http://code.google.com/p/dot-emacs/source/browse/trunk/wenbinye/config/20-func.el
(define-generic-mode 'vimrc-generic-mode
    '("\"") ;; Comment start
    '()
    '(("^[\t ]*:?\\(!\\|ab\\|map\\|unmap\\)[^\r\n\"]*\"[^\r\n\"]*\\(\"[^\r\n\"]*\"[^\r\n\"]*\\)*$"
       (0 font-lock-warning-face))
      ("\\(^\\|[\t ]\\)\\(\".*\\)$"
      (2 font-lock-comment-face))
      ("\"\\([^\n\r\"\\]\\|\\.\\)*\""
       (0 font-lock-string-face)))
    '("/vimrc\\'" "\\.vim\\(rc\\)?\\'")
    '((lambda ()
        (modify-syntax-entry ?\" ".")))
    "Generic mode for Vim configuration files.")