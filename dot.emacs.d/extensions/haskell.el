;;; Haskell
;;;
;;; http://www.haskell.org/haskellwiki/Haskell_mode_for_Emacs
;;; http://projects.haskell.org/haskellmode-emacs/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; haskell-mode
;;
;; - C-c C-l: load current buffer; starts GHCi if not started.
;; - C-c C-t: show type of name at point.
;; - C-u C-c C-t: insert type of name at point.
;; - M-.: jump to def of name at point.
;; - M-*: jump back to where you were.
;; - f8: jump to (next block of) imports.
;; - C-u f8: jump back to last place you were before first f8.
;; - TAG: cycle indentation.
;; - M-TAB: smart completions after using `C-c C-l`.
;;
;;   After starting the completion cycle, you can use plain `TAB` to
;;   get further completions, even after adding and deleting
;;   characters. If you complete the name completely, you can see the
;;   type in the echo area -- assuming `haskell-doc-mode` is enabled
;;   -- and then delete and continue completing. Would be nice to just
;;   see all the types in the completion list itself, maybe via
;;   `company` ...
;;
;; - M-{n,p}: jump between highlighted error from `C-c C-l`.


;;; Misc.

;; Create "declarations" menu with list of all decls and imports in
;; module. Not sure if there are special keyboard commands; can use
;; `M-` d` to access menu from keyboard.
(add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)

;; Make `C-c C-l` load project into GHCi. Need this enabled for other
;; features to work, e.g. `C-u C-c C-t` to insert type.
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

;; Show doc strings on idle. THIS INTERFERES WITH COMPANY-MODES'
;; TYPESIGS AT BOTTOM: NEED TO ONLY RUN THIS WHEN NOT DOING A COMPANY
;; COMPLETION? However, <f1> will still show the types for
;; completions. Also, this collision only happens when prefix being
;; completed is itself a valid name (e.g. completing 'map' to 'mapM').
;;
;; This also conflicts with error print outs from `M-{n,p}` after
;; `C-cC-l` in Haskell mode.
;;
;; TODO: figure out how to make doc mode compatible with other modes
;; that use the minibuffer.

;;(add-hook 'haskell-mode-hook 'haskell-doc-mode)

(add-hook 'haskell-mode-hook 'flyspell-prog-mode)

;; Disable electric-indent-mode, which is enabled by default in Emacs
;; 24.4, and is very annoying haskell-mode (e.g. it attempts to indent
;; every top-level definition :P).
(add-hook 'haskell-mode-hook '(lambda () (electric-indent-mode 0)))

(nc:custom-set-variable haskell-tags-on-save t)
;; Sometimes when I get prompted to remove unnecessary imports,
;; nothing happens when I enter "y"; it just hangs.
;;
;; E.g., the hang happens in `log-analyze` package.

;; (nc:custom-set-variable
;;   haskell-process-suggest-remove-import-lines t)

;; Use Hoogle to suggest missing imports?
(nc:custom-set-variable
  haskell-process-suggest-hoogle-imports t)

;; Create TAGS on every save.
(nc:custom-set-variable haskell-tags-on-save t)

;; The default is `auto`, which tries to guess. However, this isn't
;; much better, since if it can't find a sandbox it assumes you want a
;; global Cabal install. What we need is a way to be explicit about
;; the project config, e.g. defined in a `dir-locals.el` file.

;; (nc:custom-set-variable haskell-process-type 'cabal-repl)

;; I think this is supposed to make the inferior GHCi auto load
;; imported modules, but it appears this already happens ... need to
;; figure out exactly what this does.
;;
;;(nc:custom-set-variable
;;  haskell-process-auto-import-loaded-modules t)

;;; indentation modes

;; the three indentation modes are mutually exclusive ... and i don't
;; know where the differences are documented :PA

;; this one is supposed to be the most fancy.
;;(add-hook 'haskell-mode-hook 'haskell-indentation-mode)

;; this one just tabs to the beginning of the next token on the
;; previous line, like text-mode.
;;(add-hook 'haskell-mode-hook 'haskell-simple-indent-mode)

;; this one has the "insert the function name and a space" behavior
;; when tabbing under a function def, which the "fancy" one
;; (`haskell-indentation-mode`) does not.
(add-hook 'haskell-mode-hook 'haskell-indent-mode)

;; Indent 2 spaces
(nc:custom-set-variable haskell-indent-offset 2)

;;; GHCi prompt.

;; Avoid custom prompt trouble.  Instead, I factored out the prompt
;; into ~/.ghc/ghci-prompt.conf, which is loaded by `nc:ghci`.  Kind
;; of annoying but e.g. the below does not work, since the
;; -ignore-dot-ghci causes ghci to ignore the subsequent -ghci-script
;; (bug?).  If I ever want to avoid the `nc:ghci` hack, I might look
;; at the variables:
;;
;;   inferior-haskell-send-command
;;   inferior-haskell-wait-for-prompt
;;   comint-prompt-regexp
;;
;; defined by inf-haskell.el and comint mode.

;;(setq haskell-program-name "ghci -ignore-dot-ghci -ghci-script ~/.ghc/non-prompt.ghci")

;;; Keyboard shortcuts.

;; Copied from example Haskell Mode `init.el` here:
;; https://github.com/haskell/haskell-mode/wiki/Example-init.el.
(defun nc-haskell-hook ()
  ;; Use `C-u [f8]` to jump back.
  (define-key haskell-mode-map [f8] 'haskell-navigate-imports)
  ;; Contextually do clever things on the space key, in particular:
  ;;   1. Complete imports, letting you choose the module name.
  ;;   2. Show the type of the symbol after the space.
  (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)
  ;; Indent the below lines on columns after the current column.
  (define-key haskell-mode-map (kbd "C-M-<right>")
    (lambda ()
      (interactive)
      (haskell-move-nested 1)))
  ;; Same as above but backwards.
  (define-key haskell-mode-map (kbd "C-M-<left>")
    (lambda ()
      (interactive)
      (haskell-move-nested -1)))
)
(add-hook 'haskell-mode-hook 'nc-haskell-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ghc-mod
;;
;; http://www.mew.org/~kazu/proj/ghc-mod/en/preparation.html
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; - completions with C-M-i, if that key combo is not taken by
;;   flyspell-mode.
;;
;; The M-t works in many cases, e.g. to insert a module for a name at
;; point. To insert pragmas, when they are breaking compilation, you
;; can use C-cC-l: you'll be prompted if you want to add the missing
;; LANGUAGE pragma at the top. Doesn't work for all pragmas though.

(when nil

(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company-mode and company-ghc
;;
;; TODO: move some of this to it's own file?
;;
;; Using company-ghc completions: https://github.com/iquiw/company-ghc
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Use 'M-x company-complete' to start completing. It happens
;; automatically sometimes though ... UPDATE: after a configurable
;; (0.5 second default) delay, when you've typed at least a
;; configurable number (default 3) of character.
;;
;; - M-x company-complete: manually start completion.
;;
;; - Press <f1> to see haddoc for current completion.
;;
;; - Press C-w to see source (can't figure out how to make this work
;;   ... UPDATE: it works for *local* modules)
;;
;; - M-x company-ghc-complete-in-module: prompts for a module and
;;   restricts completions to that.
;;
;; - M-x company-ghc-complete-by-hoogle: prompts for a hoogle search
;;   and completes with results.
;;
;; Look at https://www.fpcomplete.com/user/DanBurton/content/ide-and-linters

(when nil

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)


;; The 'dabbrev' part means to also complete from buffers in a stupid
;; way. I don't understand why I can't complete from the current
;; buffer, even when my code compiles.
(add-to-list 'company-backends '(company-ghc :with company-dabbrev-code))
;;(add-to-list 'company-backends 'company-ghc)

;; Don't complete arbitrary strings from other buffers. Want to
;; complete arbitrary strings in current buffer, since 'ghc-mod' does
;; not provide that?
(setq company-dabbrev-code-other-buffers nil)
(setq company-show-numbers t)

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flycheck and flycheck-haskell

(when nil

;; See Haskell section of
;; http://www.flycheck.org/manual/latest/Supported-languages.html#Supported-languages
;; for info on Haskell related FlyCheck stuff.
;;
;; Using FlyCheck:
;; http://www.flycheck.org/manual/latest/Usage.html#Usage
(add-hook 'after-init-hook #'global-flycheck-mode)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))
;; How long to wait before compiling. There are other triggers; see
;; `C-h v flycheck-check-syntax-automatically`.
(setq flycheck-idle-change-delay 5.0)

(global-set-key (kbd "M-n") 'next-error)
(global-set-key (kbd "M-p") 'previous-error)


;; from cabal-cargs
;;
;;    for f in hs_source_dirs ghc_options default_extensions default_language cpp_options c_sources cc_options extra_lib_dirs extra_libraries ld_options include_dirs includes build_depends package_db autogen_hs_source_dirs autogen_include_dirs autogen_includes hdevtools_socket; do echo "======================================================================"; echo $f; echo; ~/tmp/cabal-cargs/.cabal-sandbox/bin/cabal-cargs --only=$f; echo; done
;;
;;

;; Flycheck can't handle Cabal macros like `MIN_VERSION_base`. This
;; fixes the problem for `haskell-ghc`, but not for `haskell-hlint`.
;;
;; UPDATE: there is some progress on this:
;; https://github.com/flycheck/flycheck/issues/713;
;; https://github.com/flycheck/flycheck-haskell/issues/39
(defun nc:fix-flycheck ()
  (interactive)
  (setq flycheck-ghc-args
	(split-string
	 (shell-command-to-string
	  "~/tmp/cabal-cargs/.cabal-sandbox/bin/cabal-cargs"))))
;; But `haskell-hlint` is annoying anyway, so disable it. It can warn
;; about unused PRAGMAs though, which is actually useful. May be
;; worthwhile to figure out how toonly enable that.
(setq-default flycheck-disabled-checkers '(haskell-hlint))

)
