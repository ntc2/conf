;; See `./helm.el'.
(eval-when-compile
  (require 'use-package))
(require 'diminish)

;; Clean up whitespace on edited lines on save. This allows you to
;; never add accidental EOL whitespace in Git commits, but also not
;; remove EOL whitespace in code you didn't change.
(use-package ws-butler
  :commands ws-butler-mode
  :diminish ws-butler-mode
  :init
  (add-hook 'prog-mode-hook #'ws-butler-mode))

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
(nc:custom-set-variable standard-indent 2)
(nc:custom-set-variable sh-basic-offset 2)
(nc:custom-set-variable sh-indentation 2)
(nc:custom-set-variable c-basic-offset 2)
;; java
;;
;; make arguments indented only 2 spaces past function, when all
;; function args on subsequent lines.  Good for
;; reallyLongJavaMethodNames.
;;
;; setting the c-style messes up the indent distance (c-basic-offset),
;; so reset after setting c-style.
(add-hook 'java-mode-hook
          (lambda ()
            (progn
              (c-set-style "linux")
              (setq c-basic-offset 2))))

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

;; Replace "smart" quotes with ASCII quotes. Useful in LaTeX.
(defun nc:replace-smart-quotes ()
  "replace all \"smart\" quotes in buffer with regular quotes"
  (interactive)
  (dolist (from-to '(("’" . "'") ("‘" . "'") ("”" . "\"") ("“" . "\"")))
    (let ((from (car from-to))
          (to (cdr from-to)))
      (save-excursion
        (beginning-of-buffer)
        (while (search-forward from nil t)
          (replace-match to nil t))))))

;; From https://www.emacswiki.org/emacs/UnfillParagraph
;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun nc:unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))
;; The standard `fill-paragraph' is bound to `M-q'.
(define-key global-map "\M-Q" 'nc:unfill-paragraph)
