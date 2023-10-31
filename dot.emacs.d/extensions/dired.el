;; Can now use C-x C-j followed R followed by C-x C-k to rename the
;; current buffer and its underlying file.
;;
;; See
;; http://stackoverflow.com/questions/384284/how-do-i-rename-an-open-file-in-emacs
(require 'dired-x)

(defun nc/treemacs-quit ()
  "Workaround for buggy treemacs-quit function when in peek mode."
  (interactive)
  (treemacs-peek-mode -1)
  (treemacs-quit))

;; Treemacs: graphical tree based file browser
;;
;; The docs are good:
;; - customization vars: https://github.com/Alexander-Miller/treemacs
;; - unbound functions: https://github.com/Alexander-Miller/treemacs#unbound-functions
;; - default keys: https://github.com/Alexander-Miller/treemacs#default-keymaps
(use-package treemacs
  :ensure t
  :defer t
  :pin "melpa-stable"
  :bind (("C-c t" . treemacs)
         (:map treemacs-mode-map
               ;; By default q is bound to treemacs-quit, but that
               ;; doesn't do the right thing with temp buffers in peek
               ;; mode. See
               ;; https://github.com/Alexander-Miller/treemacs/issues/1063
               ("q" . nc/treemacs-quit)))
  :config
  (add-hook 'treemacs-mode-hook
            (lambda ()
              ;; Peek mode toggle is already bound to P by default.
              (treemacs-peek-mode 1)
              ;; Follow mode is annoying: you constantly lose your
              ;; position in the tree.
              (treemacs-follow-mode -1))))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t
  :defer t
  :pin "melpa-stable")

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t
  :defer t
  :pin "melpa-stable")
