;; From the bottom of the `use-package' readme:
;; https://github.com/jwiegley/use-package.
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; Apparently you can use `use-package' instead of e.g. Cask to auto
;; install packages. See `use-package' docs for `:ensure' at
;; https://github.com/jwiegley/use-package. This is what Adam does in
;; his Emacs config at
;; https://github.com/acfoltzer/.emacs.d/blob/master/init.el.
(use-package helm
  ;; Defining `:commands' or `:bind' turns on lazy loading.
  :commands helm-mode
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x C-b" . helm-buffers-list) ;; This doesn't seem to work.
         ("C-x b" . helm-mini)
         ("M-y" . helm-show-kill-ring)
         ("C-h f" . helm-apropos)
         ("C-h a" . helm-apropos))
  ;; This runs before the package is loaded.
  :init
  (progn
    (use-package helm-descbinds
      :bind (("C-h b" . helm-descbinds)))
    
    (when nil
    ;; This is realllllly slow. Or helm is just really slow??? Still
    ;; slow with this disabled: as in it takes a second to narrow
    ;; completions???
    (progn
      ;; Maximal fuzzy matching:
      ;; https://github.com/emacs-helm/helm/wiki/Fuzzy-matching
      (setq helm-mode-fuzzy-match t
            helm-completion-in-region-fuzzy-match t)))))
