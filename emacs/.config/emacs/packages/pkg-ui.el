;;; -*- lexical-binding: t -*-

;; Theme (loaded by your ui.el or manually)
(use-package catppuccin-theme
  :defer t)

;; Collapse minor modes into a menu (nice with many packages)
(use-package minions
  :custom
  (mode-line-modes-delimiters nil)
  (minions-mode-line-lighter " …")
  :config
  (minions-mode 1))

;; Prettier mode-line segments (keep minimal)
(use-package moody
  :config
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

;; Nyan cat in mode-line (works as long as you don't override mode-line-format)
(use-package nyan-mode
  :after moody
  :custom
  (nyan-bar-length 10)
  (nyan-wavy-trail t)
  :config
  (nyan-mode 1))

(provide 'pkg-ui)
