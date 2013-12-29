; Refs
; - http://stackoverflow.com/questions/8717924/emacs-backup-automatically-disabled-for-version-controlled-files?rq=1
; - http://www.emacswiki.org/emacs/ForceBackups
; - http://www.emacswiki.org/emacs/BackupEachSave
; - http://www.gnu.org/software/emacs/manual/html_node/emacs/Backup.html
;
; Based on http://www.emacswiki.org/emacs/ForceBackups.
;
; The action is in 'backup-buffer' in 'files.el'.  Look there if you
; want to make further customizations.
;
; If backing up large files becomes a problem, try setting
; 'backup-enable-predicate' to limit by size, probably by extending
; 'normal-backup-enable-predicate'.

; Make all backups in same place: Avoid clutter, and danger of typing
; 'rm -rf * ~' instead of 'rm -rf *~' when removing clutter. See
; http://www.gnu.org/software/emacs/manual/html_node/emacs/Backup.html
; for some explanation of when backups are created, and some ways to
; force backup creation.
;
; NB: The backup dir specified here is created if it doesn't exist.
; This is bad if this creation happens while you're sudo'd since then
; root owns the backup dir ...
(nc:custom-set-variable backup-directory-alist '(("" . "~/.emacs.d/backup/per-save")))

; Backup versioned files.
;
; This is obviously not very useful for the last committed version,
; but is useful for intermediate saves.
(nc:custom-set-variable vc-make-backup-files t)

(nc:custom-set-variable version-control t)     ;; Use version numbers for backups.
(nc:custom-set-variable kept-new-versions 10)  ;; Number of newest versions to keep.
(nc:custom-set-variable kept-old-versions 0)   ;; Number of oldest versions to keep.
(nc:custom-set-variable delete-old-versions t) ;; Silently delete excess backup versions.
;; Need this for the double backup trick below to work.  Without it,
;; the first 'backup-buffer' renames the file and then the second
;; 'backup-buffer' does nothing because '(file-exists-p
;; buffer-file-name)' -- part of the test in 'backup-buffer' --
;; becomes nil.
(nc:custom-set-variable backup-by-copying t) ;; Copy all files, don't rename.
;; (nc:custom-set-variable backup-by-copying-when-linked t) ;; Copy linked files, don't rename.

(defun force-backup-of-buffer ()
  ;; Make a special "per session" backup at the first save of each
  ;; emacs session.
  (when (not buffer-backed-up)
    (let ((backup-directory-alist '(("" . "~/.emacs.d/backup/per-session")))
          (kept-new-versions 3))
      (backup-buffer)))
      ;; Mark buffer as modified so the 'backup-buffer' below will
      ;; run.
      ;; (set-buffer-modified-p t)))
  ;; Make a "per save" backup on each save.
  (let ((buffer-backed-up nil))
    (backup-buffer)))

(add-hook 'before-save-hook  'force-backup-of-buffer)
