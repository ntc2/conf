;; I'm using straight.el + use-package to manage packages, after
;; having trouble with package.el + use-package. Note that straight.el
;; suggests removing all package.el related stuff, which includes not
;; using `:pin' or `:ensure' with use-package. I haven't read the docs
;; generally yet, just learned a few commands I needed to port my old
;; setup. Main docs:
;;
;; https://github.com/radian-software/straight.el
;;
;; There is no automatic upgrading. Note that things may break on
;; upgrade. To manually update one package, e.g. after I change its
;; source pin, use `straight-pull-package', and then restart Emacs. To
;; update all packages, use `straight-pull-all', and then restart. But
;; I might want to try "freezing" first, so I can revert if
;; needed. Update commands:
;;
;; https://github.com/radian-software/straight.el#version-control-operations
;;
;; I didn't read about it yet, but freeze/thaw is available:
;;
;; https://github.com/radian-software/straight.el#configuration-reproducibility

;; Install `straight'.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package.
(straight-use-package 'use-package)
(require 'use-package)
(use-package straight
  :custom
  ;; Use straight to install packages in use-package. This has the
  ;; effect of automatically `:ensure'ing all packages, but using
  ;; actual `:ensure' is not supported afaict, since it implies some
  ;; package.el interaction.
  (straight-use-package-by-default t))

;; Install misc deps. Some of these should be moved to corresponding
;; extension files.
;;
;; Here'a a repo with a very elaborate config that seems well
;; organized using use-package, with a local packages for each
;; extension. Using local packages would help with possible
;; interdependency problems, by `require'ing any deps:
;;
;; https://github.com/jeremyf/dotemacs
;;
;;;; ================================================================

;; Git IDE.
(use-package magit)

;; Markdown.
;;
;; More recent than the version that ships with Emacs.
(use-package markdown-mode )
;; For syntax highlighting in "GFM fenced code blocks", e.g.
;;
;; ``` haskell
;; module Test where
;; ```
(use-package polymode)
(use-package poly-markdown)

;; Use-package
;;
;; https://github.com/jwiegley/use-package
;;
;; At work straight.el has been suggested as a Cask replacement.
(use-package diminish)

;; Helm.
;;
;; https://github.com/emacs-helm/helm
(use-package projectile)
(use-package helm)
(use-package helm-descbinds)
(use-package helm-projectile)
(use-package helm-ag)

;; Trim whitespace at end of edited lines.
(use-package ws-butler)

;; Edit Firefox/Chrome text areas in Emacs.
(use-package atomic-chrome)

;; Move buffers with ease.
(use-package buffer-move)

;; Typescript (used in Haddock to generate JS)
(use-package typescript-mode)

;; To get `debian-changelog-mode'.
;;(use-package dpkg-dev-el)

;; To get `racket-mode'.
(use-package racket-mode)

;; Unfill paragraphs
(use-package unfill)

;; Zenburn color theme
(use-package zenburn-theme)

;; IDE
(use-package yasnippet)
(use-package company)
(use-package flycheck)
