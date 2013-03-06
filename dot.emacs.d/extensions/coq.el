;; Force two column mode (might be better to set the "threshold" ...):
;;
;; http://proofgeneral.inf.ed.ac.uk/htmlshow.php?title=Proof+General+user+manual+(latest+release)&file=releases%2FProofGeneral-latest%2Fdoc%2FProofGeneral%2FProofGeneral_9.html#Display-customization
(nc:custom-set-variable proof-three-window-mode-policy 'hybrid)

;; Make "." behave like "C-c C-RET".
(nc:custom-set-variable proof-electric-terminator-enable t)

;; Stop proof general from inserting new lines when I enter ".".
(nc:custom-set-variable proof-next-command-insert-space nil)

;; Make "C-c C-RET" work consistently: over ssh the "C-RET" becomes
;; "RET".
(add-hook 'coq-mode-hook
  (lambda ()
    (local-set-key (kbd "C-c RET") 'proof-goto-point)))
