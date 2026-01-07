;;; -*- lexical-binding: t -*-

(use-package saveplace
  :ensure nil              ;; built-in
  :init
  (setq save-place-file (expand-file-name ".places" user-emacs-directory))
  :config
  (save-place-mode 1))

(provide 'pkg-saveplace)
