;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode line
;;
;; A recurring problem I have is that there is too much stuff in my mode
;; line and most of it gets cut off. So, I moved the mode list to a new
;; header line. This freed up a lot of space, so I changed the buffer
;; display to just be the file name (with minimal uniquification), and
;; added the full base path separately.
;;
;; I implemented the full base path to replace "$HOME" with "~", which
;; helps in most cases, but I might also want to implement a length limit
;; on the base path, truncating a prefix when it's too long. The tricky
;; part about doing that correctly is that I'd like the max length to
;; depend on the available space in the mode line. The mode line docs
;; mentioned a function that renders a mode line and returns it as a
;; string, so I could probably use that to compute the available space.
;;
;; It seems the buffer name gets artificially padded with spaces when
;; it's short, which isn't ideal, but also not a big deal.

(column-number-mode t)
;;(display-time)

;; Don't show modes in modeline
(setq-default mode-line-format (delq 'mode-line-modes mode-line-format))
;; Don't show git info in modeline.
(advice-add 'vc-git-mode-line-string
            :override (lambda (file) ""))

(defun nc:buffer-file-name ()
  "Like `buffer-file-name', but collapse '$HOME' to '~'"
  (replace-regexp-in-string (getenv "HOME") "~" (buffer-file-name)))

(when nil
;; See
;;
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Mode-Line-Format.html#Mode-Line-Format
(nc:custom-set-variable mode-line-format
  '("" ;; The leading string means "recursively eval and concat other args".
    mode-line-front-space
    mode-line-mule-info
    mode-line-client
    mode-line-modified
    mode-line-remote
    mode-line-frame-identification
    mode-line-buffer-identification "(" (:eval (nc:buffer-file-name)) ")"
    " "
    mode-line-position
    mode-line-misc-info))

;; Put the modes on top, because they never fit on my screen. Another
;; option would be to filter or shorten them somehow.
(nc:custom-set-variable header-line-format
  '("" mode-line-modes))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Uniquify buffer names.

(when nil
(nc:custom-set-variable uniquify-buffer-name-style 'post-forward-angle-brackets)
;; Always show at least two directory components, even if not
;; necessary for disambiguation.
(nc:custom-set-variable uniquify-min-dir-content 2)
)

(when nil
(require 'uniquify)
(nc:custom-set-variable uniquify-buffer-name-style 'nil)
)
