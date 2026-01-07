;;; -*- lexical-binding: t -*-
(require 'package)

(setq package-enable-at-startup nil)

(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(unless package--initialized
  (package-initialize))

;; Only refresh if needed (still happens on first run)
(unless package-archive-contents
  (package-refresh-contents))

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; --------------------------------------------------
;; Completion & navigation
;; --------------------------------------------------

(use-package vertico
  :init (vertico-mode 1))

(use-package savehist
  :init (savehist-mode 1))

(use-package consult
  :defer t)

;; --------------------------------------------------
;; Utilities
;; --------------------------------------------------

(use-package magit :defer t)

(use-package ace-window :defer t)

(use-package company
  :hook (after-init . global-company-mode))

(use-package flycheck
  :hook (after-init . global-flycheck-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred))

(use-package org-roam
  :after org)

(use-package treemacs :defer t)

(use-package catppuccin-theme :defer t)

;; Multiple cursors
(use-package multiple-cursors
  :bind (("C->"     . mc/mark-next-like-this)
         ("C-<"     . mc/mark-previous-like-this)
         ("C-c C->" . mc/mark-all-like-this)))

;; --------------------------------------------------
;; UI
;; --------------------------------------------------

(use-package minions
  :custom
  (mode-line-modes-delimiters nil)      ;; <- fixed typo
  (minions-mode-line-lighter " …")
  :config
  (minions-mode 1))

(use-package moody
  :config
  ;; Keep this minimal; don’t overwrite mode-line-format yet.
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package nyan-mode
  :after moody
  :custom
  (nyan-bar-length 10)
  (nyan-wavy-trail t)
  :config
  (nyan-mode 1))

(provide 'packages)
