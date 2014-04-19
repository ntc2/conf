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

;; Install packages as needed.
;;
;; The "Distributed Setup" section (6.3) of the El-get info
;; documentation shows a more complex setup where some packages are
;; customized with ':after' sections in a magic 'el-get-sources'
;; variable.  For example, they install 'emacs-goodies-el' via
;; 'apt-get' using a ':type apt-get' source.
;;
;; The 'el-get' command does not perform any updating; that must be
;; done manually with 'el-get-update'.
(if (not (executable-find "makeinfo"))
    (error "%s" "You need to install 'makeinfo' with 'sudo aptitude install texinfo'")
  (el-get 'sync '("haskell-mode")))
