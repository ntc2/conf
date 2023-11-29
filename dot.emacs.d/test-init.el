;; Trick taken from
;; https://github.com/rksm/emacs-rust-config/blob/master/standalone.el:
;; install Emacs deps in a temp dir, to experiment with new configs
;; without nuking existing install.
;;
;; To use this, check out a tmp copy of this config repo, edit the
;; configs, and run Emacs with this file as init:
;;
;;     emacs -Q --load <path to tmp copy of conf repo>/dot.emacs.d/test-init.el --debug-init
;;
;; The deps will be installed in
;; /var/tmp/emacs-test-init/<absolute path to tmp copy of conf repo>.
;;
;; This downloads a bunch of packages, so be sure to have a good
;; internet connection. I may have also had issues with my IP being in
;; Indonesia, and using a VPN to come from Japan instead solved
;; that. Could have been a fluke tho.

;; Set test-init.el as the init file (typically ~/.emacs).
(setq user-init-file (or load-file-name (buffer-file-name)))
;; Compute the root of the conf repo. The init file is at
;; <conf-root>/dot.emacs.d/test-init.el, so we need to go up two
;; levels.
(setq conf-root (file-name-directory
                 (directory-file-name
                  (file-name-directory user-init-file))))
;; Define user-emacs-directory as full path of absolute-path version
;; of conf-root appended to "/var/tmp/emacs-test-init/".
(setq user-emacs-directory
      (expand-file-name (substring (expand-file-name conf-root) 1)
                        "/var/tmp/emacs-test-init"))
;; Print use-emacs-directory to stdout and quit
(print (format "usage-emacs-directory: %s" user-emacs-directory)
       #'external-debugging-output)
;; Where to save `custom-set-variable's and such.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; Load the actual init file, taken from the tmp conf repo.
(load-file (expand-file-name "dot.emacs" conf-root))
