;; To install Agda:
;;
;; 1. install the type checker: cabal install agda
;; 2. install the std lib: download [1] and untar into
;;    ~/local/opt/agda-std-lib
;;
;;    I untarred lib-0.6.tar.gz, moved it to
;;    ~/local/opt/agda-std-lib-0.6, and made a symlink
;;    agda-std-lib -> agda-std-lib-0.6.
;;
;; [1] http://wiki.portal.chalmers.se/agda/pmwiki.php?n=Libraries.StandardLibrary

;; Load Agda mode if it exists.
;;
;; Based on
;; http://wiki.portal.chalmers.se/agda/pmwiki.php?n=Main.README-2-3-0
;; which does not check for existence.
;;
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Locating-Files.html
(when (executable-find "agda-mode")
  (load-file (let ((coding-system-for-read 'utf-8))
               (shell-command-to-string "agda-mode locate"))))

;; Add std lib to agda path.
;;
;; Can't use relative paths, so use 'file-truename' to canonicalize.
;;
;; http://stackoverflow.com/questions/291976/relative-path-to-absolute-path-in-elisp
(nc:custom-set-variable
 agda2-include-dirs
 `("." ,(file-truename "~/local/opt/agda-std-lib/src")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use agda input mode in non-agda modes (thanks Larry!).

;; Larry's code:
;;
;; (defun run-agda-input-hook ()
;;   (require 'agda-input)
;;   (make-local-variable 'default-input-method)
;;   (set-input-method "Agda")
;;   (agda-input-setup))
;; (add-hook 'org-mode-hook 'run-agda-input-hook)

(defun nc:init-toggle-agda-input ()
  (interactive)
  (unless (boundp 'nc:toggle-agda-input-initialized)
    (require 'agda-input)
    (make-local-variable 'nc:toggle-agda-input-initialized)
    (make-local-variable 'default-input-method)
    (setq default-input-method "Agda")
    (agda-input-setup)))

;; After running this once to turn Agda input mode on the first time,
;; you can use 'C-\' for subsequent toggles. Alternatively, could run
;; 'nc:toggle-agda-input-init' once, and then use 'C-\' to toggle to
;; turn Agda input mode on the first time.  Simplest may be to set
;; ''default-input-method' to "Agda" by default whenever '(require
;; 'agda-input)' succeeds.
(defun nc:toggle-agda-input ()
  (interactive)
  (nc:init-toggle-agda-input)
  (toggle-input-method))
