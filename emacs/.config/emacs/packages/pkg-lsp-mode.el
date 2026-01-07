;;; -*- lexical-binding: t -*-

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  ;; Prefer built-in completion (company is already global)
  (setq lsp-completion-provider :capf
        lsp-keymap-prefix "C-c l")
  :hook
  ((c-mode c++-mode python-mode) . lsp-deferred))

(provide 'pkg-lsp-mode)