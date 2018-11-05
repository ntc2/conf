(package-initialize)
(require 'polymode)
(require 'markdown-mode)
;; Requiring `poly-markdown' here doesn't seem to be necessary when
;; `poly-markdown' is installed, but it will fail loudly if
;; `poly-markdown' is not installed.
(require 'poly-markdown)
(add-to-list 'auto-mode-alist '("\\.md\\'" . poly-markdown-mode))
