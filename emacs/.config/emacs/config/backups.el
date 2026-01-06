(defconst my/backup-dir
  (expand-file-name "backups/" user-emacs-directory))

(make-directory my/backup-dir t)

(setq backup-directory-alist
      `(("." . ,my/backup-dir)))

(setq auto-save-file-name-transforms
      `((".*" ,my/backup-dir t)))

(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(provide 'backups)
