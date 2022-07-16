;;; Dependencies -- Install Emacs dependencies using Cask.

;;; Commentary:

;; Emacs package management.
;;
;; The internet seemed to think that El-Get could install MELPA
;; packages, but I could not figure out how.  The packages.el system,
;; which is native in Emacs 24, supports MELPA.  El-Get makes it very
;; easy to set up your own packages (e.g. my old manual haskell-mode
;; install is a few lines in El-Get) and the MELPA haskell-mode is way
;; out of date, so it seems good to stick with El-Get.  But it would
;; be nice if I could combine El-Get and MELPA? Maybe the answer is to
;; use both El-Get and packages.el?
;;
;; Update: I'm now trying Cask ...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

;; https://github.com/cask/cask/issues/463#issuecomment-794249642
(setq warning-suppress-log-types '((package reinitialization)))
(require 'cask "~/local/opt/cask.git/cask.el")
(cask--initialize)

;; Add dependencies in `~/.emacs.d/Cask` and then
;;
;;   `nc:emacs:cask install`
;;
;; to only install new deps, or
;;
;;   `nc:emacs:cask update`
;;
;; to install all deps with newer versions available.
