(require 'polymode)
(require 'markdown-mode)
;; Requiring `poly-markdown' here doesn't seem to be necessary when
;; `poly-markdown' is installed, but it will fail loudly if
;; `poly-markdown' is not installed.
(require 'poly-markdown)

;; Copied from `poly-markdown'. The default math inner modes are not
;; activating for me, so I'm adding my own.
(define-polymode nc/poly-markdown-mode
  :hostmode 'poly-markdown-hostmode
  :innermodes '(poly-markdown-fenced-code-innermode
                ;; poly-markdown-inline-code-innermode ;; This was already disabled.
                ;; poly-markdown-displayed-math-innermode ;; This doesn't work.
                ;; poly-markdown-inline-math-innermode ;; This also doesn't work.
                nc/poly-markdown-displayed-math-2-dollars-innermode
                nc/poly-markdown-displayed-math-begin-end-innermode
                nc/poly-markdown-inline-math-innermode
                poly-markdown-yaml-metadata-innermode))
;; Create aliases for latex mode in different contexts, so I can match
;; on them with `derived-mode-p' or `(eq major-mode <alias>)'.
(define-derived-mode nc/inline-latex-mode latex-mode "Inline LaTeX"
  "A mode for editing inline LaTeX that behaves like latex-mode.")
(define-derived-mode nc/display-latex-mode latex-mode "Display LaTeX"
  "A mode for editing display LaTeX that behaves like latex-mode.")
;; Use `re-builder' to figure out the regexps!
(define-innermode nc/poly-markdown-displayed-math-2-dollars-innermode
  :mode 'nc/display-latex-mode
  :head-matcher (cons "\\(?:\n\n\\|^> \\)\\(\\$\\$\\)\n" 1)
  :tail-matcher (cons "\n\\(\\$\\$\\)\n"  1)
  :allow-nested nil)
(define-innermode nc/poly-markdown-displayed-math-begin-end-innermode
  :mode 'nc/display-latex-mode
  :head-matcher (cons "^\\(\\$\\)\\\\begin" 1)
  :tail-matcher (cons "\\\\end{[^}]*}\\(\\$\\)"  1)
  :allow-nested nil)
(define-innermode nc/poly-markdown-inline-math-innermode
  :mode 'nc/inline-latex-mode
  :head-matcher (cons "\\(?:^\\| \\)\\(\\$\\)[^\$]" 1)
  :tail-matcher (cons "[^\\$]\\(\\$\\)" 1)
  :allow-nested nil)
(add-to-list 'auto-mode-alist '("\\.md\\'" . nc/poly-markdown-mode))

;; Disable visual-fill-column-mode in display-math LaTeX blocks, when
;; cursor is in the block.
(defun nc/poly-disable-visual-fill-for-latex (old new)
  "Disable visual-fill-column-mode in LaTeX inner modes."
  ;; Check if the current major mode of the buffer is `latex-mode`
  (if (eq major-mode 'nc/display-latex-mode)
      (visual-fill-column-mode -1)  ;; Disable visual-fill-column-mode
    (visual-fill-column-mode 1)))   ;; Else, enable visual-fill-column-mode
(add-hook 'polymode-after-switch-buffer-hook 'nc/poly-disable-visual-fill-for-latex)
