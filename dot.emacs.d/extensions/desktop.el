;; Based on
;; http://www.xsteve.at/prg/emacs/power-user-tips.html
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Emacs-Sessions.html
;;
;; The desktop save file is called '.emacs.desktop'.
;;
;; Use 'M-x desktop-save' once to save the desktop.  When it exists,
;; Emacs updates it on every exit.  The desktop is saved in the
;; directory where you started emacs, i.e. you have per-project
;; desktops.  If you ever save a desktop in your home dir, then that
;; desktop will be the default in the future when you start emacs in a
;; dir with no desktop.  See the ``desktop'' docs for more info.

(desktop-save-mode 1)

;; Places to look for desktop files, '.emacs.desktop'.
(nc:custom-set-variable desktop-path '("." "~/.emacs.d/"))

;; Type and amount of history to save in the desktop.
(setq desktop-globals-to-save
      (append '((extended-command-history . 30)
                (file-name-history        . 100)
                (grep-history             . 30)
                (compile-history          . 30)
                (minibuffer-history       . 50)
                (query-replace-history    . 60)
                (read-expression-history  . 60)
                (regexp-history           . 60)
                (regexp-search-ring       . 20)
                (search-ring              . 20)
                (shell-command-history    . 50)
                tags-file-name
                register-alist)))
