;; See :/dot.emacs.d/extensions/helm.el for `use-package' docs.
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(use-package dkpg-dev-el
  :commands debian-changelog-mode)
