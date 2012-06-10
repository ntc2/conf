;; Tabs
;;
;; I created a .dir-locals.el to configure tabs on git-annex, with
;; contents, but there were issues with unsafe command:
;; `(highlight-regexp "^ *")' for highlighting suspicious leading
;; spaces.
;;
;; ;; Configure emacs' treatment of tabs.
;; ;;
;; ;; See
;; ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html
;; ;; for a description of this file.
;; ;;
;; ;; The 'nil' below applies to all modes.
;; ((nil . ((indent-tabs-mode . t)
;;         (tab-width . 2)))
;;  (haskell-mode . (
;;         ;; Highlight leading space characters, to avoid indenting with
;;         ;; spaces.
;;         ;;
;;         ;; Emacs will prompt you about this, saying it's unsafe, but
;;         ;; you can permanently store an exception by pressing "!",
;;         ;; which inserts
;;         ;;
;;         ;;   (safe-local-variable-values . (quote ((eval highlight-regexp "^ *"))))
;;         ;;
;;         ;; in your ~/.emacs ... except the exception doesn't work, and
;;         ;; emacs still asks you on each file you open :P
;;         (eval . (highlight-regexp "^ *")))))

;; !!!: 'setq' does not work for 'tab-width'.
(nc:custom-set-variable tab-width 2)

;; Don't use tabs to indent.
(nc:custom-set-variable indent-tabs-mode nil)

;; * Show pointless whitespace.
;;
;; ** at end of line:
;;
;; (setq-default show-trailing-whitespace t)
;;
;; Unfortunately, that highlights as you type, which is really
;; annoying ... but the while-you-type face is a different color than
;; the on-another-line face, so the former can be turned off using
;; 'custom-set-faces' (found with M-x describe-face):
;;
;; (custom-set-faces
;;   '(show-ws-trailing-whitespace ((t nil))))
;; (nc:custom-set-face
;;    show-ws-trailing-whitespace ((t nil)))
;;
;; ** at end of file:
(nc:custom-set-variable indicate-empty-lines t)

;; Let searches span multiple lines.
;;
;; Make regexp search (C-M-s) handle line wrapping intelligently. NB:
;; after starting a regexp search, you can use regular C-s to jump to
;; the next match
;; 
;; But how to get the same for non-regexp search (C-s)?  Could rebind
;; regular search to regexp search?
(nc:custom-set-variable search-whitespace-regexp "[ \t\r\n]+")

;; Delete pointless whitespace in region
(defun nc:del-ws ()
  (interactive)
  (whitespace-cleanup-region))

;;; Fix it's-all-text gmail buffers.
(defun nc:replace-nbsp ()
  "replace all non-breaking spaces (ASCII char 160; HTML &nbsp;)
in buffer with regular spaces"
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (while (search-forward " " nil t)
      (replace-match " " nil t))))

