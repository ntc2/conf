;; See :/dot.emacs.d/extensions/helm.el for `use-package' docs.
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(use-package web-mode
  :commands web-mode
  :mode "\\.html\\'"
  :custom
  (web-mode-script-padding 0 "Don't left pad indent <script> blocks."))
