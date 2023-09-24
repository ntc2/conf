;; Trick taken from
;; https://github.com/rksm/emacs-rust-config/blob/master/standalone.el:
;; install Emacs deps in a temp dir, to experiment with new configs
;; without nuking existing install.
;;
;; Run Emacs with this file as init:
;;
;;     emacs -Q --load ~/v/conf/dot.emacs.d/test-init.el
(setq user-init-file (or load-file-name (buffer-file-name)))
;; Install deps here.
(setq user-emacs-directory "/var/tmp/emacs-test-init")
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;;(load-file "~/v/conf/dot.emacs.d/extensions/00-dependencies.el")
(load-file "~/v/conf/dot.emacs")
