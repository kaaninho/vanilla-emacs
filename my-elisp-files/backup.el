(setq make-backup-files t      ; backup of a file the first time it is saved.
      auto-save-default t      ; auto-save every buffer that visits a file
      version-control t        ; Use version numbers for backups.
      kept-new-versions 10     ; Number of newest versions to keep.
      kept-old-versions 6      ; Number of oldest versions to keep.
      delete-old-versions t    ; Don't ask to delete excess backup versions.
      backup-by-copying t
      auto-save-timeout 20     ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200   ; number of keystrokes between auto-saves (default: 300)
      ) 


;; Default and per-save backups go here:
(setq backup-directory-alist '(("" . "~/.backup/per-save")))


(defun force-backup-of-buffer ()
  ;; Make a special "per session" backup at the first save of each
  ;; emacs session.
  (when (not buffer-backed-up)
    ;; Override the default parameters for per-session backups.
    (let ((backup-directory-alist '(("" . "~/.backup/per-session")))
	  (kept-new-versions 3))
      (backup-buffer)))
  ;; Make a "per save" backup on each save.  The first save results in
  ;; both a per-session and a per-save backup, to keep the numbering
  ;; of per-save backups consistent.
  (let ((buffer-backed-up nil))
    (backup-buffer)))

(add-hook 'before-save-hook  'force-backup-of-buffer)
