;; Use fly-spell
;(erc-spelling-mode 1)

;; Log all irc activity.
;;
;; See http://www.emacswiki.org/emacs/ErcLogging#toc5.
;;
;; Logs go in ~/log by default.
;;
;; After 'erc-save-buffer-in-logs' has run at least once, can run
;; again with 'C-cC-l'.
;;
;; Saves log on every insert into the Erc buffer. Here "insert" means
;; "change what is visible", so the buffer gets saved on every new
;; message, not just messages typed by me.
(add-hook 'erc-insert-post-hook 'erc-save-buffer-in-logs)
