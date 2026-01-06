;;; -*- lexical-binding: t -*-
(require 'package)

(setq package-enable-at-startup nil)

(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(unless package--initialized
  (package-initialize))

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

(use-package consult :defer t)

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
  :commands lsp)

(use-package org-roam
  :after org)

(use-package treemacs
  :defer t)

(use-package catppuccin-theme
  :defer t)

(provide 'packages)
