;; See :/dot.emacs.d/extensions/helm.el for comments explaining
;; `use-package'.
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; We disable all of the auto indent stuff in typescript mode, because
;; it doesn't respect the style of existing code.

(defun nc:disable-electric-indent ()
  ;; Setting this in `config' below, instead of in this hook, doesn't
  ;; work ...
  (setq electric-indent-inhibit t))

(use-package typescript-mode
  :commands typescript-mode
  :mode "\\.tsx?\\'"
  :init
  (add-hook 'typescript-mode-hook 'nc:disable-electric-indent)
  :config
  (progn
    (nc:custom-set-variable typescript-indent-level
                            2)
    (nc:custom-set-variable typescript-auto-indent-flag
                            nil)))
