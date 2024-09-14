;; Perhaps this file is poorly named: it's named by what part of the
;; Emacs UI it affects, not by what it actually does ...

;; See vc.el for diff-hl configuration, which needs to load after magit.

;; Disable scroll bars: takes up space.
(scroll-bar-mode 0)

;; Add line numbers to wide windows.
(defun nc/maybe-linum ()
  (if (and (or (derived-mode-p 'prog-mode)
               (derived-mode-p 'text-mode))
           (not (or
                 (derived-mode-p 'org-mode)
                 (derived-mode-p 'magit-mode)))
           (>= (window-width) 70))
      (progn
        (display-line-numbers-mode 1))
    (progn
      (display-line-numbers-mode -1))))
(add-hook 'window-configuration-change-hook #'nc/maybe-linum)
