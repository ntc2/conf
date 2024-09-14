;; See ../test-init.el for sandboxing config changes, including upgrading all
;; packages.
;;
;; I'm using straight.el + use-package to manage packages, after
;; having trouble with package.el + use-package. Note that straight.el
;; suggests removing all package.el related stuff, which includes not
;; using `:pin' or `:ensure' with use-package. I haven't read the docs
;; generally yet, just learned a few commands I needed to port my old
;; setup. Main docs:
;;
;; https://github.com/radian-software/straight.el
;;
;; There is no automatic upgrading of packages; note that things may break on
;; upgrade. To manually upgrade one package, e.g. after I change its source pin,
;; or just to get latest upstream version, use `straight-pull-package', and then
;; restart Emacs. To update all packages, use `straight-pull-all' (this takes a
;; long time!), and then restart. These operations are smart about not
;; overwriting any local changes you might have. Package upgrade docs:
;;
;; https://github.com/radian-software/straight.el#version-control-operations
;;
;; To get a consistent setup across machines, and be able to revert, use
;; freeze/thaw. To freeze, use `straight-freeze-versions', and then commit the
;; updated versions file in
;; `~/v/conf/dot.emacs.d/straight/versions/default.el'. To revert to the state
;; specified in the versions file, use `straight-thaw-versions'. Freeze/thaw docs:
;;
;; https://github.com/radian-software/straight.el#configuration-reproducibility
;;
;; To edit a package, e.g. to fix a bug, just edit the package directly in the
;; straight checkout of its repo under `~/.emacs.d/straight/repos/'. Straight
;; will not overwrite your changes without warning. Local editing docs:
;;
;; https://github.com/radian-software/straight.el#edit-packages-locally

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

;; Edit Firefox/Chrome text areas in Emacs. Also to install GhostText
;; extenion on the Firefox/Chrome side, and run special Emacs with
;; `nc:ghosttext` shell command. Only need one instance of
;; `nc:ghosttext` for all browser text area editing.
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

;; Good discussion of keymaps here:
;; https://www.masteringemacs.org/article/mastering-key-bindings-emacs#keymap-lookup-order
(defun nc/next-error ()
  "Jump to the next error in the current buffer, where 'error' is
defined by what minor modes are in use.

Specifically, in order of precedence:
- if `smerge-mode' is active, jump to the next merge conflict;
- if `flycheck-mode' is active, jump to the next flycheck error;
"
  (interactive)
  (cond
   ((bound-and-true-p smerge-mode) (smerge-next))
   ;; Make it easier to jump between errors; the default `C-c ! n' is
   ;; painful.
   ((and (bound-and-true-p flycheck-mode)
         flycheck-current-errors)
    (flycheck-next-error))
   (t (next-error))))
(defun nc/previous-error ()
  "Like `nc/next-error'."
  (interactive)
  (cond
   ((bound-and-true-p smerge-mode) (smerge-prev))
   ((bound-and-true-p flycheck-mode) (flycheck-previous-error))
   (t (previous-error))))
(use-package emacs
  :custom
  (fill-column 80 "Set in use-package emacs. Default is 70 columns.")
  :bind
  ("C-<prior>" . nc/previous-error)
  ("C-<next>" . nc/next-error)
  )
