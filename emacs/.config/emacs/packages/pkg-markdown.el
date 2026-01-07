;;; -*- lexical-binding: t -*-

(defun my/markdown-mode-setup ()
  "Setup for markdown buffers."
  (set-fill-column 80))

(defun my/markdown-demote-or-header (orig-fun &rest args)
  "If point is not on markdown structure, insert header; otherwise demote."
  (if (not (or (thing-at-point-looking-at markdown-regex-header-atx)
               (thing-at-point-looking-at markdown-regex-header-setext)
               (thing-at-point-looking-at markdown-regex-hr)
               (markdown-cur-list-item-bounds)
               (markdown-table-at-point-p)
               (thing-at-point-looking-at markdown-regex-bold)
               (thing-at-point-looking-at markdown-regex-italic)))
      (markdown-insert-header-atx-1)
    (apply orig-fun args)))

(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.text\\'" . markdown-mode))
  :hook (markdown-mode . me/markdown-mode-setup)
  :custom
  (markdown-asymmetric-header t)
  :bind (:map markdown-mode-map
              ("M-<left>"  . markdown-promote)
              ("M-<right>" . markdown-demote)
              ("M-["       . markdown-promote)
              ("M-]"       . markdown-demote))
  :config
  (advice-add #'markdown-demote
              :around
              #'me/markdown-demote-or-header))

(provide 'pkg-markdown)
