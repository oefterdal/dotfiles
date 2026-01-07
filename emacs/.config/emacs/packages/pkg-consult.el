;;; -*- lexical-binding: t -*-

(use-package consult
  :bind (("C-x b" . consult-buffer)
         ("C-s"   . consult-line)
         ("C-c p s" . consult-ripgrep)))

(provide 'pkg-consult)
