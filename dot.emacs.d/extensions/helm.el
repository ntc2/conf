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
;; See also: Adam uses `allout' mode to use an outline mode (the
;; `;;;_' prefixes) with his init file. This might be a nice
;; alternative to my splitting things into many files, since the
;; reason for that was to ease navigation and management.
;;
;; The Helm maintainer's Emacs config:
;; https://github.com/thierryvolpiatto/emacs-tv-config/blob/master/init-helm-thierry.el
(use-package helm
  ;; Defining `:commands' or `:bind' turns on lazy loading.
  :commands helm-mode
  :diminish helm-mode
  :defer t
  ;; Force eager loading. This seems to fix my complete-as-I-type
  ;; problem! Got the idea that lazy loading could be the problem
  ;; here: https://github.com/emacs-helm/helm/issues/173. Now, if I
  ;; could just get Helm to highlight the substring matches.
  ;;
  ;;:demand
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x C-b" . helm-buffers-list)
         ("C-x b" . helm-mini)
         ("M-y" . helm-show-kill-ring)
         ("C-h f" . helm-apropos)
         ("C-h a" . helm-apropos)
         ("C-h v" . helm-apropos))
  :custom
  ;; Use `C-c h' as the prefix for Helm commands.
  (helm-command-prefix-key "C-c h" "Mnemonic Helm prefix.")

  ;; This runs before the package is loaded.
  :init
  (progn
    (use-package helm-config)

    ;; Supposed to make Helm adapt to my usage.
    (use-package helm-adaptive
      :config (helm-adaptive-mode 1))

    (use-package helm-descbinds
      :bind (("C-h b" . helm-descbinds)))

    (use-package helm-projectile)

    ;; Maximal fuzzy matching:
    ;; https://github.com/emacs-helm/helm/wiki/Fuzzy-matching
    (setq helm-mode-fuzzy-match t
          helm-completion-in-region-fuzzy-match t)
    ;; Make `helm-find' use `-path' patterns instead of `-name'
    ;; patterns when invoking `find'.
    (setq helm-findutils-search-full-path t)
    ;; Turn on helm mode right away, so that e.g. `DefaultPath'
    ;; completion in `helm-find' (via `C-u C-c h /') works.
    (helm-mode 1))

  :config
  (progn
    (setq
     ;; Don't prompt before creating new files.
     helm-ff-newfile-prompt-p nil
     ;; Don't truncate buffer names in helm buffer list.
     helm-buffer-max-length nil
     helm-buffers-truncate-lines nil)
    ;; Bind tab to file name completion so that it's more like
    ;; traditional `C-xC-f'. Note that Right also completes, and Left
    ;; removes path components. And swap other bindings accordingly
    ;; (but I don't think I ever use `helm-select-action' on purpose
    ;; ...).
    (bind-key "<tab>" 'helm-execute-persistent-action helm-map)
    (bind-key "C-i"   'helm-execute-persistent-action helm-map)
    (bind-key "C-z"   'helm-select-action helm-map)))

;; Use `C-c p h' to run `projectile-find-file'. Unlike plain Helm,
;; this completes as you type, and shows full paths. Since I mostly
;; work in projects, this will do what I want in practice, if I can
;; remember to run it instead of `C-x C-f' (maybe I could make that
;; map to `projectile-find-file' when in a Projectile project?).
(use-package projectile
  :defer t
  ;; :ensure t
  :commands projectile-mode
  :diminish projectile-mode

  :custom
  (projectile-file-exists-remote-cache-expire nil "Don't cache remote files, to avoid getting prompted for SSH passwords where keys are not available.")
  ;; The leading '*' means ignore not just at the top-level. It does
  ;; *not* mean ignore any dir whose name ends with the string after
  ;; the '*' :P
  ;;
  ;; See: https://github.com/bbatsov/projectile/pull/1153
  (projectile-globally-ignored-directories
   (append '("*.svn" "*.git" "*.stack-work" "*dist" "*dist-newstyle")
           projectile-globally-ignored-directories))
  ;; This doesn't seem to take effect until you delete
  ;; `~/.emacs.d/projectile-bookmarks.eld', or remove the offending
  ;; project from it. In particular, the
  ;; `projectile-cleanup-known-projects' doesn't not remove these :/
  (projectile-ignored-projects
   (append '("~/.emacs.d/" "/tmp/") projectile-ignored-projects))

  :init
  (progn
    (projectile-mode +1)
    ;; Use `C-c p' as projectile command prefix.
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
    (setq projectile-completion-system 'helm)
    ;; The default is the `alien' indexing method, but that ignores
    ;; the settings in the .projectile file. In particular, the
    ;; .projectile file can be used to override the .gitignores.
    ;;
    ;; WHAT DOESN'T WORK: The docs say to add
    ;;
    ;;   !/path/to/be/unignored
    ;;
    ;; to .projectile to add a Git-ignored file to the project. This
    ;; doesn't appear to actually work though in my SFE project, and I
    ;; tried several variations ... the problem seems to be related to
    ;; the fact that when a directory is ignored by Git, git doesn't
    ;; list the contents. I tried explicitly including the contents of
    ;; the ignored directory with things like
    ;;
    ;;   !/ignored/dir/*
    ;;
    ;; but no dice.
    ;;
    ;; WHAT DOES WORK: projectile also has an "explicit include" mode,
    ;; where the default is to not include any files in the project,
    ;; and you specify directories to include using '+<dir>'. This can
    ;; be combined with '-<pattern>' to then ignore some stuff that
    ;; '+<dir>' included. A few confusing points: the explicit include
    ;; with "+" only applies to directories, not individual files,
    ;; according to the docs. However, it seems the "!<pattern>" might
    ;; allow including individual files, altho as I said above I
    ;; couldn't make it work for me.
    ;;
    ;; In any case, this setup in the '.projectile' file works for me:
    ;;
    ;;   +.
    ;;   +/deps/<submodule> ;; One such line for each submodule :P
    ;;   +/ignored/dir
    ;;
    ;; where the '+.' means "include everything that Git is tracking"
    ;; and then I additionally add the directory that Git is ignoring
    ;; to the project.
    ;;
    ;; Documentation:
    ;; https://docs.projectile.mx/en/latest/projects/#ignoring-files
    (setq projectile-indexing-method 'hybrid)
    ;; Need to manually manage stale caches, and doesn't work with
    ;; subversion that needs password.
    ;;
    (setq projectile-enable-caching t)
    (helm-projectile-on))

  :config
  (progn
    ;; This is bound in the Projectile map, i.e. `C-c p s s' runs this
    ;; ag command. Need to learn how to edit the results like Adam
    ;; showed me. I can bring up an editable results buffer with f4,
    ;; but then search and replace in it doesn't seem to work. The
    ;; `helm-ag' is a separate package, so docs might help.
    (bind-key "s s" 'helm-projectile-ag projectile-command-map)))
