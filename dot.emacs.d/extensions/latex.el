;; Enable RefTex
;; - http://www.gnu.org/software/auctex/manual/reftex/index.html
;; - http://emacswiki.org/emacs/AUCTeX#toc2
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'bibtex-mode-hook 'turn-on-reftex)
;; and connect with Auctex (not sure what this does)
(setq reftex-plug-into-AUCTeX t)
;; NB: the hook is *not* called 'latex-mode-hook', e.g. i got really
;; confused when this didn't work:

;(add-hook 'latex-mode-hook
;          (lambda () (reftex-mode t)))
