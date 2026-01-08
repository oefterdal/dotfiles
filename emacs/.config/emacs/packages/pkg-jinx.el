;;; -*- lexical-binding: t -*-

(use-package jinx
  :hook (text-mode . jinx-mode)
  :bind (("M-$" . jinx-correct)))

(provide 'pkg-jinx)
