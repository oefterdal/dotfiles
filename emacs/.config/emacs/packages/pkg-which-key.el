;;; -*- lexical-binding: t -*-

(use-package which-key
  :config
  (which-key-mode 1)
  (setq which-key-idle-delay 0.3
        which-key-side-window-location 'bottom)

  (which-key-add-key-based-replacements
    "C-c !" "diagnostics"
    "C-c p" "project"
    "C-c g" "git"
    "C-c f" "files"
    "C-c w" "windows"))

(provide 'pkg-which-key)
