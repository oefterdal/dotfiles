;;; -*- lexical-binding: t -*-

(use-package project
  :ensure nil  ;; built-in
  :bind (("C-x p p" . project-switch-project)
         ("C-x p f" . project-find-file)
         ("C-x p g" . project-find-regexp))) ;; fallback if consult not loaded

;; If consult is available, use the nicer consult commands
(with-eval-after-load 'consult
  (global-set-key (kbd "C-x p b") #'consult-project-buffer)
  (global-set-key (kbd "C-c p s") #'consult-ripgrep))

(provide 'pkg-project)
