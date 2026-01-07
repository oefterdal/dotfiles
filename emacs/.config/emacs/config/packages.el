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

;; Better matching everywhere
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        ;; keep file completion sane
        completion-category-overrides '((file (styles partial-completion)))))

;; Rich annotations in minibuffer lists (M-x, find-file, etc.)
(use-package marginalia
  :init
  (marginalia-mode 1))

;; Context actions on minibuffer candidates + things at point
(use-package embark
  :bind (("C-." . embark-act)         ;; do something with current target/candidate
         ("C-;" . embark-dwim)        ;; best default action
         ("C-h B" . embark-bindings)) ;; show bindings for current target
  :init
  ;; Make Embark use completing-read UI (Vertico)
  (setq prefix-help-command #'embark-prefix-help-command))

;; Nice integration: export consult results to an Embark buffer, etc.
(use-package embark-consult
  :after (embark consult))

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

;; Which key
(use-package which-key
  :config
  (which-key-mode 1)
  (setq which-key-idle-delay 0.3
        which-key-side-window-location 'bottom))

(use-package embark
  :bind (("C-." . embark-act)))

(use-package embark-consult
  :after (embark consult))

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
