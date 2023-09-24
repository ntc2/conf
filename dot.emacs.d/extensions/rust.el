;; Based on / copied from
;; https://robert.kra.hn/posts/rust-emacs-setup/#rust-emacs-configuration-in-detail,
;; which is regularly updated (as of 2023-09-20 it was most recently
;; updated on 2023-07-23).
;;
;; Below code based on
;; https://github.com/rksm/emacs-rust-config/commit/ec562f005152fabba0447ce64687cbb572a7d49b.

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; rustic = basic rust-mode + additions

(use-package rustic
  :ensure
  :pin "melpa"
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status)
              ("C-c C-c e" . lsp-rust-analyzer-expand-macro)
              ("C-c C-c d" . dap-hydra)

              ;; Open docs in browser for thing at point.
              ("C-c C-c h w" . lsp-rust-analyzer-open-external-docs)
              ;; Pop-up temp docs that disappear when you move the cursor.
              ("C-c C-c h g" . lsp-ui-doc-glance)
              ;; Pop-up persistent docs in new/other window.
              ("C-c C-c h o" . lsp-describe-thing-at-point))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; for rust-analyzer integration

;; NC: many of these packages (company, lsp, lsp-ui) are also loaded
;; in the haskell config. It's possible their settings can
;; conflict. I'm not sure if multiple `use-package' calls for the same
;; package is officially supported, or what the semantics are for
;; merging.

(use-package lsp-mode
  :ensure
  :pin "melpa"
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  ;; When this is `t' you get a big annoying popup at the bottom of
  ;; the screen. When set to `nil' you just get the type of thing at
  ;; point. https://emacs-lsp.github.io/lsp-mode/page/settings/mode/#lsp-eldoc-render-all
  (lsp-eldoc-render-all nil)
  (lsp-idle-delay 0.6)
  ;; This controls the overlays that display type and other hints inline. Enable
  ;; / disable as you prefer. Will require a `lsp-workspace-restart' to have an
  ;; effect on open projects.
  ;;
  ;; See https://emacs-lsp.github.io/lsp-mode/page/lsp-rust-analyzer/
  ;; for all available settings, including some other goodies like a
  ;; special smart join-line function, and a way to run tests for
  ;; thing at point automatically.
  ;;
  ;; NOTE: those docs mention that inlay hints don't get along with
  ;; "lsp-ui sideline", which is the thing displaying the types on the
  ;; RHS. But I'm not experiencing any trouble, besides the general
  ;; clutter that I'm not sure is worth it.
  ;;
  ;; Need `lsp-inlay-hint-enable' here and `lsp-inlay-hints-mode' to
  ;; get inlay hints working.
  (lsp-inlay-hint-enable t)
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  ;; Turning this on doesn't do anything afaict?
  (lsp-rust-analyzer-display-parameter-hints t)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (add-hook 'lsp-mode-hook 'lsp-inlay-hints-mode))

;; Docs: https://emacs-lsp.github.io/lsp-ui/
(use-package lsp-ui
  :ensure
  :pin "melpa"
  :commands lsp-ui-mode
  :custom
   ;; I can't figure out what "peek" is doing.
  (lsp-ui-peek-always-show t)
  ;; This "show-hover" shows the package for each symbol on line. I
  ;; don't find this useful, especially when I can hover on a symbol
  ;; to get all it's info.
  (lsp-ui-sideline-show-hover nil)
  ;; This solves the "what code action is available when there's only
  ;; 1" problem, at the cost of some screen clutter. Might be better
  ;; to find a way to optionally list available code actions, e.g. by
  ;; making `lsp-execute-code-action' always prompt user, even if only
  ;; one code action is available. There are some related commands in
  ;; this SO answer, but I didn't test them:
  ;; https://emacs.stackexchange.com/a/77533/9977.
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-doc-enable t))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; Create / cleanup rust scratch projects quickly

;;(use-package rust-playground :ensure)


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; for Cargo.toml and other config files

(use-package toml-mode :ensure :pin "melpa")
