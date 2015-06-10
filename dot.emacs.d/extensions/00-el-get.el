;; Emacs package management.
;;
;; The internet seemed to think that El-Get could install MELPA
;; packages, but I could not figure out how.  The packages.el system,
;; which is native in Emacs 24, supports MELPA.  El-Get makes it very
;; easy to set up your own packages (e.g. my old manual haskell-mode
;; install is a few lines in El-Get) and the MELPA haskell-mode is way
;; out of date, so it seems good to stick with El-Get.  But it would
;; be nice if I could combine El-Get and MELPA? Maybe the answer is to
;; use both El-Get and packages.el?
;;
;; Install el-get automatically if not already installed.  This will
;; be annoying if I don't have an internet connection, but I only
;; expect to run it once (so maybe I should run in manually?). It
;; takes a few minutes the first time.
;;
;; Also, install specified packages if non-installed.
;;
;; Based on "Basic Setup" section of README.md at
;; https://github.com/dimitri/el-get
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Override the default paths.
;;
;; See https://raw.github.com/dimitri/el-get/master/el-get-install.el
;; for variable usage.

;; Override the default 'el-get-dir' in 'el-get-install.el'.
;;
;; The directory structure is '<el-get-dir>/<package>'. So,
;; e.g. 'el-get' package is under '~/local/opt/el-get/el-get.
(setq el-get-dir "~/local/opt/el-get")

(add-to-list 'load-path "~/local/opt/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;; I'm not sure what this is for yet ...
;(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

;; El-get looks here for init code for installed packages. See the
;; "User Init" section of the El-get info docs.
(nc:custom-set-variable el-get-user-package-directory "~/.emacs.d/extensions/el-get")

;(el-get 'sync)

;; Packages to install
;;
;;
;; Based on the "Distributed Setup" section (6.3) of the El-get info
;; documentation. For example, they install 'emacs-goodies-el' via
;; 'apt-get' using a ':type apt-get' source.

;; This is a magic variable used by El-Get.
(setq el-get-sources
      '(
        ;; This recipe works, but dart-mode is broken in Emacs 23 :P
        ;; Supposed to work in Emacs 24 though.

        ;; (:name dart-mode
        ;;  :type git
        ;;  :url "https://github.com/nex3/dart-mode"
        ;;  :after (add-to-list 'auto-mode-alist '("\\.dart\\'" . dart-mode)))
      ))

(setq nc:el-get-packages
      (append
       '("el-get"
         "haskell-mode"
         ;; Comes with Emacs 24.
         "package"
         "llvm-mode"
         "rust-mode")
       (mapcar 'el-get-source-name el-get-sources)))

;; Install packages as needed.
;;
;; The 'el-get' command does not perform any updating; that must be
;; done manually with 'el-get-update'.
(if (not (executable-find "makeinfo"))
    (error "%s" "You need to install 'makeinfo' with 'sudo aptitude install texinfo'")
  ;; Need to include "el-get" in the list of packages or it's info
  ;; documentation does not get loaded!
  (el-get 'sync nc:el-get-packages))
