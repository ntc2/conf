;; Based on http://melpa.milkbox.net/#/getting-started .

(require 'package)
(add-to-list 'package-archives
  ;; The 't' means to append, so that MELPA comes after the more
  ;; stable ELPA archive.
  '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Add ELPA if necessary. Looking at the El-Get package.rcp recipe in
;; ~/local/opt/el-get/recipes it seems this is probably unnecessary.
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

(require 'el-get-elpa)
;; Build the El-Get copy of the package.el packages if we have not
;; built it before.  Will have to look into updating later ...
(unless (file-directory-p el-get-recipe-path-elpa)
  (el-get-elpa-build-local-recipes))
