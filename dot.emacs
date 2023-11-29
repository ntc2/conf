;; -*- emacs-lisp -*-

;; We're using straight.el.
(setq package-enable-at-startup nil)

;;; default font

;; An article on open source programming fonts:
;; https://opensource.com/article/17/11/how-select-open-source-programming-font
;;
;; Article recommends, in preference order: FiraCode, Inconsolata,
;; DejaVu Sans Mono, Source Code Pro, Noto Mono
;;
;; Some font packages on Ubuntu 22.04:
;;
;; fonts-inconsolata
;; fonts-monoid?
;; fonts-mononoki?
;; fonts-noto-mono (but the zero and oh are not well distinguished :/)
;; fonts-proggy?
;; fonts-firacode
;; fonts-recommended: depends on firacode and mononoki and several others, curated by debian fonts team
;; fonts-terminus
;; ttf-mscorefonts-installer: install non-free ms fonts, e.g. consolas
;;
;; The firacode readme mentions these alternatives:
;;
;; <quote>
;; 190 ### Alternatives
;; 191
;; 192 Free monospaced fonts with ligatures:
;; 193
;; 194 - [Hasklig](https://github.com/i-tu/Hasklig)
;; 195 - [Monoid](http://larsenwork.com/monoid/)
;; 196 - [Fixedsys Excelsior](https://github.com/kika/fixedsys)
;; 197 - [Iosevka](https://be5invis.github.io/Iosevka/)
;; 198 - [DejaVu Sans Code](https://github.com/SSNikolaevich/DejaVuSansCode)
;; 199 - [Victor Mono](https://rubjo.github.io/victor-mono/)
;; 200 - [Cascadia Code](https://github.com/microsoft/cascadia-code)
;; 201 - [JetBrains Mono](https://github.com/JetBrains/JetBrainsMono)
;; </quote>
;;
;; Can use e.g. `(member "Monaco" (font-family-list))' to check if a
;; font (here "Monaco") is available and print an error message or try
;; another.
;;
;; Some fonts I tried:
;;
;;(set-face-attribute 'default nil :font "DejaVu Sans Mono-12")
;;(set-face-attribute 'default nil :font "Mononoki-14")
;;(set-face-attribute 'default nil :font "Noto Mono-14")
;;(set-face-attribute 'default nil :font "Inconsolata-14")
;; To actually get ligatures with FiraCode, you need to enable
;; ligatures. See https://github.com/mickeynp/ligature.el for
;; instructions.

;; In Emacs 27.1 Terminus-14 works, but in Emacs 28.1 it's tiny, so
;; switched back to FiraCode.
(set-face-attribute 'default nil :font "FiraCode-12")
;; FiraCode was good, but I think I prefer the crisp look of bitmap
;; (non-antialiased) fonts, like Terminus. Trying bold in hopes it's
;; easier on the eyes, altho this loses some info that used to be
;; distinguished by selective bolding when the default wasn't bold.
;;
;; Terminus doesn't play well with lsp-ui: the stuff lsp
;; adds to the right side of the screen wraps. Existing bug report
;; here: https://github.com/emacs-lsp/lsp-ui/issues/285.
;;(set-face-attribute 'default nil :font "Terminus-14:bold")
;; Fix from above bug report.
;; (custom-set-faces
;;   '(markdown-code-face ((t (:inherit default)))))

;; Variations: terminus:bold terminus-20:bold
;;(add-to-list 'default-frame-alist '(font . "terminus"))

;; Make font bigger. Seems to just choose the corresponding -<size>
;; version of the font. But might be more predictable than using the
;; -<size> suffixes, and doesn't require knowing which -<size>s are
;; available (e.g. for Terminus where there are only a few sizes).
;;
;;(set-face-attribute 'default nil :height 150)

(apply 'load-file (file-expand-wildcards "~/v/conf/dot.emacs.d/extensions/00-dependencies.el"))

;; * Custom set variables.
;;
;; Multiple `custom-set-variables' calls can be confusing [1], but
;; having a single monolithic call is not modular.  Solution: use
;; multiple calls, but comment each variable with a comment indicating
;; where it was set.  The comment is shown when using the customize
;; interface to customize the variable.
;;
;; See `./extensions/white-space-and-punctuation.el' for example usage.
;;
;; Note: `setq' does not always work as a replacement for a
;; `custom-set-variables' entry.  E.g. `(setq tab-width 2)' has no
;; effect. The following do work:
;;
;;   (custom-set-default 'tab-width 2)
;;   (setq-default tab-width 2)
;;   (custom-set-variables '(tab-width 2))
;;
;;
;; On the other hand, it's not necessarily a good idea to use
;; `custom-set-variables' on a variable that isn't hooked into the
;; customize interface (you get a warning from customize, but I'm not
;; sure if there are any pitfalls).

;; [1]: http://www.dotemacs.de/custbuffer.html

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;(package-initialize)

(defun nc:custom-set-warning ()
  "Warning to insert in comment field of `custom-set-variable' entries."
  (format "!!! CAREFUL: CUSTOM-SET IN %s !!!" load-file-name))

;; To debug, use `pp-macroexpand-last-sexp' to show the expansion of a
;; macro call.
(defmacro nc:custom-set-variable (var value)
  "Call `custom-set-variables' with a comment warning about
customizing using the customize GUI.

XXX: does not support setting the optional NOW and
REQUEST (dependency) fields."
  `(custom-set-variables
    ;; 'load-file-name' is set by 'load':
    ;; http://stackoverflow.com/a/1971376/470844
    '(,var ,value nil nil ,(nc:custom-set-warning))))

(defmacro nc:custom-set-face (face spec)
  "XXX: untested.

See `nc:custom-set-variable'."
  (custom-set-faces
    `(,face ,spec nil ,(nc:custom-set-warning))))

(global-font-lock-mode 1)

;; Use shorter prompt for yes/no questions.
(fset 'yes-or-no-p 'y-or-n-p)

;; Remember recent files.
(recentf-mode 1)
(setq recentf-max-saved-items 100)

;; Save last cursor position in files and jump there when reopening.
(save-place-mode 1)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(case-fold-search t)
 '(current-language-environment "English")
 '(inhibit-startup-screen t)
 '(mouse-wheel-follow-mouse t)
 '(mouse-wheel-mode t nil (mwheel))
 '(ps-black-white-faces (quote ((font-lock-builtin-face "black" nil bold underline) (font-lock-comment-face "gray20" nil italic) (font-lock-constant-face "black" nil bold) (font-lock-function-name-face "black" nil bold) (font-lock-keyword-face "black" nil bold underline) (font-lock-string-face "black" nil italic) (font-lock-type-face "black" nil italic) (font-lock-variable-name-face "black" nil bold italic) (font-lock-warning-face "black" nil bold italic))))
 '(ps-line-number t)
 '(ps-print-color-p (quote black-white))
 '(rst-level-face-base-color "not-a-color-so-ill-get-black")
 '(transient-mark-mode t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

;; Don't complain about these variables when set e.g. in
;; .dir-locals.el file.
;;
(nc:custom-set-variable safe-local-variable-values
                        '((haskell-tags-on-save)))

;; Always select / jump to help window when it opens. This lets you
;; hit "q" to close the help window, without having to select it
;; first.
(nc:custom-set-variable help-window-select t)

;; Highlight matching parens.
(add-hook 'after-init-hook #'show-paren-mode)

;;; TAGS

;; Respect case in TAGS search.
(nc:custom-set-variable tags-case-fold-search nil)

;; Reread the TAGS files automatically when they change.
(nc:custom-set-variable tags-revert-without-query t)

;;; Load customizations
;;;
;;; Use a defvar to guard any configurable code, e.g. for code you
;;; only want to run conditionally, set the variable to nil in the
;;; system-custom.el to disable the guarded code. See
;;; extensions/flyspell.el for an example.

;; Emacs now (Since Emacs 24?) warns that I shouldn't add '~/.emacs.d'
;; to my path.

;; Byte-compile any out-of-date configurations and load all
;; configurations.
;;
;; The `nil' arg means only recompile when `.el' file is newer than
;; `.elc' file.
;;
;; The `0' arg means to compile `foo.el' to `foo.elc' even if there is
;; no `foo.elc' already (so "compile or recompile").
;;
;; The `t' arg means load the file after compilation.
;;
;;(require 'bytecomp)
;;(defun nc:load (f) (byte-recompile-file f nil 0 t))

;; Had problems with my `nc:custom-set-variable' macro not working
;; correctly in byte compiled files, probably since the introduction
;; of the above code. I couldn't figure out what was wrong, but it
;; seems that startup isn't any slower with using regular `.el' files,
;; so I'm not going to bother with byte compiling anymore.
(defun nc:load (f) (load-file f))
(let (;; My customizations, split up in separate files. The
      ;; `~/.emacs.d/system-custom.el' is loaded separately at the end
      ;; of this file.
      ;;
      ;; HACK: using [a-z] prefix to avoid loading
      ;; 00-dependencies.el. Better to just move that out of
      ;; extensions/ dir.
      (files (file-expand-wildcards "~/.emacs.d/extensions/[a-z]*.el")))
  (mapc 'nc:load files))
;; Generate list of extensions for (e.g. bisection) debugging config
;; probllems.
;;
;; find ~/.emacs.d/extensions/ -name '*.el' | xargs -n1 -i echo "(nc:load \"{}\")"

;; like focus follows mouse in gnome
;(setq mouse-autoselect-window t)

;; Use CUA rectangles, but not other cua bindings like `C-c' for
;; copy. Use `C-RET' to start cua rectangle mode, and `RET' in mode to
;; move cursor to different rectangle corners. Cursor motion expands
;; or contracts the rectangle by default, but `M-<cursor movement>`
;; instead moves the whole rectangle. Editing while the rectangle is
;; active results in the same action on all lines.
(cua-selection-mode 1)

;;; disable tool bar (DISABLED IN ~/.Xresources NOW)
; some mode might use this in a useful way, e.g. debuggers or web
; browsers.  special case those as necessary ... or only disable for
; specific modes ...

; e.g., something like
; (add-hook 'coq-mode-hook
;           (lambda () (tool-bar-mode t)))

;(if (functionp 'tool-bar-mode)
;    (tool-bar-mode 0))

; less direct way: tool-bar-mode only def in graphics
;(when window-system
;  (tool-bar-mode nil))

; Make M-x apropos, and maybe C-h a, show more results. This var has
; documentation *after* apropos.el loads, e.g. after using M-x
; apropos.
(nc:custom-set-variable apropos-do-all t)

; Make backspace work more often.
;
; Causes problems in text-only emacs that isn't already broken :P
; (normal-erase-is-backspace-mode 1)

; Soft wrap lines in split frames.  Lines in full width frames are
; soft wrapped by default, and lines in split frames are truncated by
; default.
(setq truncate-partial-width-windows nil)

; Make svn commits hapen in text mode. SVN commit tmp files have names
; like svn-commit.2.tmp or svn-commit.tmp.  NB: it seems the file name
; that the auto-mode regexps match against is a *full* path, so it
; doesn't work to anchor at the beginning (^).
(add-to-list 'auto-mode-alist '("svn-commit\\(\\.[0-9]+\\)?\\.tmp$" . text-mode))

; vi/less style jk navigation in view-mode.  Kind of pointless because
; du keys scroll half page.  But the default <enter>y for <down><up>
; were too annoying.
(when (boundp 'view-mode-map)
  (mapc (lambda (kv) (define-key view-mode-map (car kv) (cadr kv)))
        '(("j" View-scroll-line-forward)
          ("k" View-scroll-line-backward))))

; Make it darker
;(set-foreground-color "grey")
;(set-background-color "black")

;;; Some customization from the UW CSL .emacs

;; If you would like smooth scrolling, uncomment this line
(setq scroll-step 1)

; Not sure which modes become more decorated?
(setq font-lock-maximum-decoration t)

; The default history length, at least in sml-run, is apparently 30,
; which is pretty worthless for an interpreter.
(setq history-length 5000)

;; Make emacs shell display ascii color escapes properly.
(ansi-color-for-comint-mode-on)

;;; End CSL stuff

;;; version control

;; Stop emacs from asking: "Symbolic link to SVN-controlled source
;; file; follow link?" every time I open a symlink to a versioned
;; file.
(setq vc-follow-symlinks t)

;; System (e.g. math.wisc.edu vs uoregon.edu) *specific*
;; code.  In practice I symlink a system specific
;; versioned file here.
;;
;; Load this last, in case it overrides existing settings.
(when (file-exists-p "~/.emacs.d/system-custom.el")
    (nc:load "~/.emacs.d/system-custom.el"))

;; Show key sequence completion hints automatically on pause.
(use-package which-key
  :init
  (which-key-mode))
