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
;;
;; The Helm maintainer's Emacs config:
;; https://github.com/thierryvolpiatto/emacs-tv-config/blob/master/init-helm-thierry.el
(use-package helm
  ;; Defining `:commands' or `:bind' turns on lazy loading.
  :commands helm-mode
  ;; Force eager loading. This seems to fix my complete-as-I-type
  ;; problem! Got the idea that lazy loading could be the problem
  ;; here: https://github.com/emacs-helm/helm/issues/173. Now, if I
  ;; could just get Helm to highlight the substring matches.
  :demand
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x C-b" . helm-buffers-list) ;; This doesn't seem to work.
         ("C-x b" . helm-mini)
         ("M-y" . helm-show-kill-ring)
         ("C-h f" . helm-apropos)
         ("C-h a" . helm-apropos)
         ("C-h v" . helm-apropos))
  ;; This runs before the package is loaded.
  :init
  (progn
    ;; Without this, complete-as-you-type does not work. But then it
    ;; seems it does not work all the time ... sometimes I still have
    ;; to hit SPC before the last thing I typed is used to filter the
    ;; current match list. UPDATE: after forcing eager loading, seems
    ;; to work consistently!
    (helm-mode)
    (use-package helm-config)

    ;; Supposed to make Helm adapt to my usage.
    (use-package helm-adaptive
      :config (helm-adaptive-mode 1))

    (use-package helm-descbinds
      :bind (("C-h b" . helm-descbinds)))

    (use-package helm-projectile)

    (when nil
    ;; This is realllllly slow. Or helm is just really slow??? Still
    ;; slow with this disabled: as in it takes a second to narrow
    ;; completions???
    (progn
      ;; Maximal fuzzy matching:
      ;; https://github.com/emacs-helm/helm/wiki/Fuzzy-matching
      (setq helm-mode-fuzzy-match t
            helm-completion-in-region-fuzzy-match t)))))

;; Use `C-c p h' to run `projectile-find-file'. Unlike plain Helm,
;; this completes as you type, and shows full paths. Since I mostly
;; work in projects, this will do what I want in practice, if I can
;; remember to run it instead of `C-x C-f' (maybe I could make that
;; map to `projectile-find-file' when in a Projectile project?).
(use-package projectile
  :ensure t
  :commands projectile-global-mode
;;  :diminish projectile-mode
  :init
  (progn
    (projectile-global-mode 1)
    (setq projectile-completion-system 'helm)
    ;; Need to manually manage stale caches, and doesn't work with
    ;; subversion that needs password.
    ;;
    ;; (setq projectile-enable-caching t)
    (helm-projectile-on))

  :config
  (progn
    ;; This is bound in the Projectile map, i.e. `C-c p s s' runs this
    ;; ag command. Need to learn how to edit the results like Adam
    ;; showed me. I can bring up an editable results buffer with f4,
    ;; but then search and replace in it doesn't seem to work. The
    ;; `helm-ag' is a separate package, so docs might help.
    (bind-key "s s" 'helm-projectile-ag projectile-command-map)))