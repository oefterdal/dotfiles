;;; -*- lexical-binding: t -*-

(use-package orderless
  :init
  ;; Better matching everywhere
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        ;; Keep file completion sane
        completion-category-overrides
        '((file (styles partial-completion)))))

(provide 'pkg-orderless)
