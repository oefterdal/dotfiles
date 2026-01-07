;;; -*- lexical-binding: t -*-

;; --------------------------------------------------
;; Scrolling (reduce jumpiness)
;; --------------------------------------------------
(setq scroll-margin 5
      scroll-conservatively 101
      scroll-preserve-screen-position t)

;; --------------------------------------------------
;; Display optimizations
;; --------------------------------------------------
(defun my/setup-display-optimization ()
  (when (boundp 'redisplay-dont-pause)
    (setq redisplay-dont-pause t))
  (when (boundp 'jit-lock-defer-time)
    (setq jit-lock-defer-time 0.25))
  (when (boundp 'jit-lock-chunk-size)
    (setq jit-lock-chunk-size 100))
  (when (boundp 'display-line-numbers-width)
    (setq display-line-numbers-width 4)))

(add-hook
 'after-init-hook
 (lambda ()
   (my/setup-display-optimization)

   ;; Clean UI
   (menu-bar-mode -1)
   (tool-bar-mode -1)
   (scroll-bar-mode -1)
   (line-number-mode 1)
   (column-number-mode 1)
   (global-display-line-numbers-mode 1)
   (setq-default frame-title-format "")

   ;; Macos Only
   (set-frame-parameter nil 'ns-transparent-titlebar t)
   (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
   ;; Reuse same window for common buffers
   (add-to-list 'display-buffer-alist '("\\*Help\\*" . (display-buffer-same-window)))
   (add-to-list 'display-buffer-alist '("\\*grep\\*" . (display-buffer-same-window)))
   (add-to-list 'display-buffer-alist '("\\*Warnings\\*" . (display-buffer-same-window)))
   (add-to-list 'display-buffer-alist '("\\*Messages\\*" . (display-buffer-same-window)))

   ;; Theme
   (setq custom-safe-themes t)
   (mapc #'disable-theme custom-enabled-themes)
   (when (locate-library "catppuccin-theme")
     (load-theme 'catppuccin t))

   ;; No noise
   (setq ring-bell-function 'ignore)
   (setq visible-bell nil)))

(provide 'ui)
