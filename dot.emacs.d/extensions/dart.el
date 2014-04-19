;; The dart-mode mode is broken on Emacs 23, so use javascript-mode
;; instead.  Gives decent highlighting ...

(add-to-list 'auto-mode-alist '("\\.dart\\'" . javascript-mode))
