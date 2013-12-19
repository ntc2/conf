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

(when (require 'agda-input nil t)
  (nc:custom-set-variable default-input-method "Agda")
  ;; Any effect when running this when not in Agda input mode? Seems
  ;; that Agda input mode works just fine without it.
  (agda-input-setup))

;; Named function to help me remember that I can toggle agda mode. Can
;; just use 'C-\' instead.
(defun nc:toggle-agda-input ()
  (interactive)
  (toggle-input-method))
