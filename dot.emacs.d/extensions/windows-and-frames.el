(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configure windows and frames.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The document for `split-window-sensibly' explains how these two
;; variables interact.
;;
;; Prefer vertical (side-by-side) splits.
(setq split-height-threshold nil)
;; But fall back to horizontal (one-above-the-other) splits if
;; vertical splits would result in windows with less than 70 columns.
(setq split-width-threshold 140)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Navigation

;; use SHIFT+<arrow> to navigate windows
(windmove-default-keybindings)

;; Like `windmove', but move the buffer contents as well as the focus.
(use-package buffer-move
  :commands buf-move-left buf-move-right buf-move-up buf-move-down
  :bind (("C-S-<left>"  . buf-move-left)
         ("C-S-<right>" . buf-move-right)
         ("C-S-<up>"    . buf-move-up)
         ("C-S-<down>"  . buf-move-down)))
