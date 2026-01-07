;;; -*- lexical-binding: t -*-

(use-package dired
  :ensure nil   ;; built-in
  :commands (dired dired-jump)
  :custom
  ;; Always copy/delete recursively without asking
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)

  ;; Reuse the same buffer when navigating
  (dired-kill-when-opening-new-dired-buffer t)

  ;; Human-friendly sizes
  (dired-listing-switches "-alh")

  :bind (("C-x C-j" . dired-jump)
         :map dired-mode-map
         ("h" . dired-up-directory)
         ("l" . dired-find-file)
         ("RET" . dired-find-file)
         ("^" . dired-up-directory))

  :config
  ;; Auto-refresh when files change
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (add-hook 'dired-mode-hook #'auto-revert-mode))

;; Extra niceties (still built-in)
(use-package dired-x
  :ensure nil
  :after dired)

(provide 'pkg-dired)
