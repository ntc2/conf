;; Author: Nathan Collins <nathan.collins@gmail.com>
;;
;; Defines Proof-General-style commands and key bindings:
;;
;; * C-c C-n:   eval a declarations and move to its end.
;; * C-c C-RET: eval all declarations up to point.
;;
;; The implementation is naive and slow: we send all declarations to
;; twelf server, one by one.  A faster implementation might send a
;; 'loadFile' command to twelf server, in the style of
;; 'twelf-save-and-check-file'.
;;
;; Depends on the twelf.el provided as part of twelf distribution at
;; http://twelf.org.
;;
;; Usage: load this file in your .emacs.
;;
;; Tested with Twelf 1.7.1 distribution.

(defun twelf-check-declaration-and-move ()
  "Check declaration at point and move to its end,
displaying result from the twelf server."
  (interactive)
  ;; Only check non-comment decls. The 'twelf-check-declaration' is
  ;; "smart" and checks the next real decl when run on a comment or
  ;; whitespace.  This results in redundant checks in
  ;; 'twelf-check-declarations-to-point' if we don't look out for it.
  ;; Luckily, 'twelf-end-of-par' returns 't' iff the current decl is
  ;; "real".
  (when (twelf-end-of-par)
    ;; Here 't' means "and display server state".
    (twelf-check-declaration t)))

(defun twelf-check-declarations-to-point ()
  "Check all declarations from beginning of buffer,
until point is reached, or until there is an error."
  (interactive)
  (let ((start (nth 0 (twelf-current-decl)))
        (done nil))
    (goto-char (point-min))
    (while (not done)
      ;; (sleep-for 0.5)
      (twelf-check-declaration-and-move)
      (setq done (> (point) start)))))

;; ???: the save-excursion / save-current-buffer don't seem to
;; actually save and restore the buffer :P ???  May be related to the
;; save done by twelf-save-check-file.
(defun twelf-check-declarations-to-point2 ()
  "Check all declarations from beginning of buffer,
until point is reached, or until there is an error."
  (interactive)
  (save-current-buffer
    (save-excursion
      (twelf-end-of-par)
      (insert "%.")
      (twelf-save-check-file))))

;; Add Proof-General-style key bindings.
(add-hook 'twelf-mode-hook
  (lambda ()
    (local-set-key (kbd "C-c C-n") 'twelf-check-declaration-and-move)
    (local-set-key (kbd "C-c C-a") 'twelf-check-declarations-to-point2)
    (local-set-key (kbd "C-c <C-return>") 'twelf-check-declarations-to-point)))
