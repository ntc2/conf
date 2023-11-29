;; ================================================================
;; GitHub Copilot
;;
;; Config based on
;; https://robert.kra.hn/posts/2023-02-22-copilot-emacs-setup/
;; - https://github.com/zerolfx/copilot.el
;; - https://github.com/rksm/copilot-emacsd


;; NC: this function is unused. And I don't think I want this anyway:
;; easy enough to press <right> to accept copilot completions.
(defun rk/copilot-tab ()
  "Tab command that will complet with copilot if a completion is
available. Otherwise will try company, yasnippet or normal
tab-indent."
  (interactive)
  (or (copilot-accept-completion)
      (company-yasnippet-or-completion)
      (indent-for-tab-command)))

(defun rk/copilot-complete-or-accept ()
  "Command that either triggers a completion or accepts one if one
is available. Useful if you tend to hammer your keys like I do."
  (interactive)
  (if (copilot--overlay-visible)
      (progn
        (copilot-accept-completion)
        (open-line 1)
        (next-line))
    (copilot-complete)))

(defun rk/copilot-quit ()
  "Run `copilot-clear-overlay' or `keyboard-quit'. If copilot is
cleared, make sure the overlay doesn't come back too soon."
  (interactive)
  (condition-case err
      (when copilot--overlay
        (lexical-let ((pre-copilot-disable-predicates copilot-disable-predicates))
          (setq copilot-disable-predicates (list (lambda () t)))
          (copilot-clear-overlay)
          (run-with-idle-timer
           1.0
           nil
           (lambda ()
             (setq copilot-disable-predicates pre-copilot-disable-predicates)))))
    (error nil)))

(defun rk/copilot-complete-if-active (next-func n)
  (let ((completed (when copilot-mode (copilot-accept-completion))))
    (unless completed (funcall next-func n))))

(defun rk/no-copilot-mode ()
  "Helper for `rk/no-copilot-modes'."
  (copilot-mode -1))

(defvar rk/no-copilot-modes '(shell-mode
                              inferior-python-mode
                              eshell-mode
                              term-mode
                              vterm-mode
                              comint-mode
                              compilation-mode
                              debugger-mode
                              dired-mode-hook
                              compilation-mode-hook
                              flutter-mode-hook
                              minibuffer-mode-hook)
  "Modes in which copilot is inconvenient.")

(defvar rk/copilot-manual-mode nil
  "When `t' will only show completions when manually triggered, e.g. via M-C-<return>.")

(defvar rk/copilot-enable-for-org nil
  "Should copilot be enabled for org-mode buffers?")

(defun rk/copilot-enable-predicate ()
  ""
  (and
   (eq (get-buffer-window) (selected-window))))

(defun rk/copilot-disable-predicate ()
  "When copilot should not automatically show completions."
  (or rk/copilot-manual-mode
      (member major-mode rk/no-copilot-modes)
      (and (not rk/copilot-enable-for-org) (eq major-mode 'org-mode))
      (company--active-p)))

(defun rk/copilot-change-activation ()
  "Switch between three activation modes:
- automatic: copilot will automatically overlay completions
- manual: you need to press a key (M-C-<return>) to trigger completions
- off: copilot is completely disabled."
  (interactive)
  (if (and copilot-mode rk/copilot-manual-mode)
      (progn
        (message "deactivating copilot")
        (global-copilot-mode -1)
        (setq rk/copilot-manual-mode nil))
    (if copilot-mode
        (progn
          (message "activating copilot manual mode")
          (setq rk/copilot-manual-mode t))
      (message "activating copilot mode")
      (global-copilot-mode))))

(defun rk/copilot-toggle-for-org ()
  "Toggle copilot activation in org mode. It can sometimes be
annoying, sometimes be useful, that's why this can be handly."
  (interactive)
  (setq rk/copilot-enable-for-org (not rk/copilot-enable-for-org))
  (message "copilot for org is %s" (if rk/copilot-enable-for-org "enabled" "disabled")))

;; (eval-after-load 'copilot
;;   '(progn
;;      ;; Note company is optional but given we use some company commands above
;;      ;; we'll require it here. If you don't use it, you can remove all company
;;      ;; related code from this file, copilot does not need it.
;;      (require 'company)
;;      (global-copilot-mode)))

;; load the copilot package
(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :config
  (add-hook 'prog-mode-hook 'copilot-mode)
  (add-hook 'text-mode-hook 'copilot-mode)
  ;; Open a mode's definition and look for `define-derived-mode' to
  ;; find what mode it derives from.
  (add-hook 'conf-mode-hook 'copilot-mode)

  ;; keybindings that are active when copilot shows completions
  (define-key copilot-mode-map (kbd "M-<up>") #'copilot-next-completion)
  ;;(define-key copilot-mode-map (kbd "M-]") #'copilot-previous-completion)
  (define-key copilot-mode-map (kbd "M-<right>") #'copilot-accept-completion-by-word)
  (define-key copilot-mode-map (kbd "M-<down>") #'copilot-accept-completion-by-line)

  ;; global keybindings
  (define-key global-map (kbd "M-C-<return>") #'rk/copilot-complete-or-accept)
  (define-key global-map (kbd "M-C-<escape>") #'rk/copilot-change-activation)
  ;;(define-key global-map (kbd "<tab>") #'rk/copilot-tab)

  ;; Do copilot-quit when pressing C-g
  (advice-add 'keyboard-quit :before #'rk/copilot-quit)

  ;; Complete by pressing right or tab but only when copilot completions are
  ;; shown. This means we leave the normal functionality intact.
  (advice-add 'right-char :around #'rk/copilot-complete-if-active)
  ;; NC: this causes errors.
  ;;(advice-add 'indent-for-tab-command :around #'rk/copilot-complete-if-active)

  ;; deactivate copilot for certain modes
  (add-to-list 'copilot-enable-predicates #'rk/copilot-enable-predicate)
  (add-to-list 'copilot-disable-predicates #'rk/copilot-disable-predicate))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; auto-completion and code snippets

;; NC: for the yasnippet, I could also copy rk's Rust snippet defs
;; from his config repo. But according to lsp-rust-analyzer docs I
;; need to enable this to get things like expanding a `match' by
;; filling in all the possible constructor branches.

(use-package yasnippet
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

(use-package company
  :bind
  (:map company-mode-map
        ("C-<tab>" . company-complete))
  :config
  (add-hook 'prog-mode-hook 'company-mode))

(defun company-yasnippet-or-completion ()
  (interactive)
  (or (do-yas-expand)
      (company-complete-common)))

;; The completion stuff below seems to be cribbed from Emacs wiki. See
;; SO answer:
;; https://emacs.stackexchange.com/questions/7908/how-to-make-yasnippet-and-company-work-nicer.
(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "::") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

;; NC: not quite right: when there is both a company completion and
;; copilot completion available, tab only tries the company
;; completion.
(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (unless (copilot-accept-completion)
      (if yas/minor-mode
          (unless
              (do-yas-expand)
            (if (check-expansion)
                (company-complete-common)
              (indent-for-tab-command)))))))

;; Need to run this once per machine (?) to login.
;;(copilot-login)
