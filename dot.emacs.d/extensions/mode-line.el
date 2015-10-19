(column-number-mode t)
(display-time)

;; See
;; http://www.lunaryorn.com/2014/07/26/make-your-emacs-mode-line-more-useful.html
;; for a more principled approach; I just deleted the vc-mode stuff
;; from the default value.
(custom-set-variables '(mode-line-format
  '("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification " " mode-line-position
 mode-line-modes mode-line-misc-info mode-line-end-spaces)))

;; Uniquify buffer names.
;;
;; Note the 4th arg to the custom set variable: that means to do
;; `(require 'uniquify)`.
(custom-set-variables
  '(uniquify-buffer-name-style 'post-forward-angle-brackets
                              nil (uniquify))
  ;; Always show at least two directory components, even if not
  ;; necessary for disambiguation.
  '(uniquify-min-dir-content 2))

