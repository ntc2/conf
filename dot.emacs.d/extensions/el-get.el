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
;; Seems this results in a pointless extra 'el-get' dir, but the
;; defaults do the same ...
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
(el-get 'sync)

;; Install packages as needed. This is a no-op most of the time, so
;; make this more manual if it's too slow.
(if (not (executable-find "makeinfo"))
    (error "%s" "You need to install 'makeinfo' with 'sudo aptitude install texinfo'")
  (el-get-install "haskell-mode"))
