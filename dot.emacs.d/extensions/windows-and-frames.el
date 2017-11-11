;; Configure windows and frames.

;; The document for `split-window-sensibly' explains how these two
;; variables interact.
;;
;; Prefer vertical (side-by-side) splits.
(setq split-height-threshold nil)
;; But fall back to horizontal (one-above-the-other) splits if
;; vertical splits would result in windows with less than 70 columns.
(setq split-width-threshold 140)
