;;; -*- lexical-binding: t; -*-

;;;; --------------------------------------------------
;;;; Early startup optimizations
;;;; --------------------------------------------------

;; Reduce startup noise
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t
      inhibit-splash-screen t)

;; Aggressive GC during init
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Temporarily disable file handlers (huge speedup)
(defvar my/init-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Prefer newer elisp files
(setq load-prefer-newer t)

;; Avoid warnings during init
(setq ad-redefinition-action 'accept)

;; Start in fundamental-mode
(setq initial-major-mode 'fundamental-mode)

;; Avoid slow terminal init work
(unless (daemonp)
  (advice-add #'tty-run-terminal-initialization :override #'ignore)
  (add-hook 'window-setup-hook
            (lambda ()
              (advice-remove #'tty-run-terminal-initialization #'ignore)
              (tty-run-terminal-initialization (selected-frame) nil t))))

;;;; --------------------------------------------------
;;;; Directory layout
;;;; --------------------------------------------------

;; Keep Emacs internals in ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.emacs.d/"))

;; Custom.el separate
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file 'noerror 'nomessage))

;;;; --------------------------------------------------
;;;; Load real configuration
;;;; --------------------------------------------------

(load (expand-file-name "~/.config/emacs/init.el") nil 'nomessage)

;;;; --------------------------------------------------
;;;; Restore sane runtime defaults
;;;; --------------------------------------------------

;; Restore file handlers
(setq file-name-handler-alist my/init-file-name-handler-alist)

;; Reasonable GC after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 64 1024 1024)  ;; 64MB
                  gc-cons-percentage 0.1)))
