;; How to manually upgrade packages:
;;
;; https://emacs.stackexchange.com/a/31874/9977
;;
;; There is no automatic upgrading. Note that things may break on
;; upgrade. Would like a way to specify the previous versions an
;; optionally revert ...

;; Setup `straight'.
;; https://github.com/radian-software/straight.el
;;
;; Use this to install copilot.
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

;; Add package repos
(require 'package)
;; I don't understand why we want the Tromey ELPA?
;;(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/"))
;; The `t' means "append", so we keep the default repositories.
;;
;; Note that MELPA Stable is not what it sounds like: it's rarely
;; updated, and it's not a collection compatible packages.
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(setq package-user-dir (expand-file-name "pkgs/" user-emacs-directory))
(package-initialize)

;; Install use-package that we require for managing all other dependencies
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
;; Pin to nongnu by default.
(setq use-package-always-pin "nongnu")

;; Install various deps.
;; ================================================================

;; Git IDE.
(use-package magit :ensure t)
;; Git-gutter doesn't work well for me in Emacs 27.
(use-package diff-hl :ensure t :pin "melpa-stable")

;; Markdown.
;;
;; More recent than the version that ships with Emacs.
(use-package markdown-mode :ensure t)
;; For syntax highlighting in "GFM fenced code blocks", e.g.
;;
;; ``` haskell
;; module Test where
;; ```
(use-package polymode :ensure t :pin "melpa-stable")
(use-package poly-markdown :ensure t :pin "melpa-stable")

;; Use-package
;;
;; https://github.com/jwiegley/use-package
;;
;; At work straight.el has been suggested as a Cask replacement.
(use-package diminish :ensure t :pin "melpa-stable")

;; Helm.
;;
;; https://github.com/emacs-helm/helm
(use-package projectile :ensure t)
(use-package helm :ensure t :pin "melpa")
(use-package helm-descbinds :ensure t :pin "melpa")
(use-package helm-projectile :ensure t :pin "melpa")
(use-package helm-ag :ensure t :pin "melpa")

;; Trim whitespace at end of edited lines.
(use-package ws-butler :ensure t)

;; Edit Firefox/Chrome text areas in Emacs.
(use-package atomic-chrome :ensure t :pin "melpa-stable")

;; Move buffers with ease.
(use-package buffer-move :ensure t :pin "melpa-stable")

;; Typescript (used in Haddock to generate JS)
(use-package typescript-mode :ensure t)

;; To get `debian-changelog-mode'.
(use-package dpkg-dev-el :ensure t :pin "melpa-stable")

;; To get `racket-mode'.
(use-package racket-mode :ensure t)

;; Unfill paragraphs
(use-package unfill :ensure t :pin "melpa-stable")

;; Zenburn color theme
(use-package zenburn-theme :ensure t)

;; IDE
(use-package yasnippet :ensure t :pin "melpa-stable")
(use-package company :ensure t :pin "melpa-stable")
(use-package flycheck :ensure t :pin "melpa-stable")
