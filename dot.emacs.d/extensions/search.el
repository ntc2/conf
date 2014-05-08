
;; Let regexp searches span multiple lines.
;;
;; Make regexp search (C-M-s) handle line wrapping intelligently. NB:
;; after starting a regexp search, you can use regular C-s to jump to
;; the next match
;; 
;; But how to get the same for non-regexp search (C-s)?  Could rebind
;; regular search to regexp search? Yes!
(nc:custom-set-variable search-whitespace-regexp "[ \t\r\n]+")

;; Default to regexp search.
(global-set-key "\C-s" 'isearch-forward-regexp)
(global-set-key "\C-r" 'isearch-backward-regexp)
