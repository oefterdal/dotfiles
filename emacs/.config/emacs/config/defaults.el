;; Startup Performance
(setq inhibit-startup-message t)      		        ;; Skip startup message
(setq inhibit-startup-echo-area-message t)  	    ;; Skip echo area messages
(setq inhibit-splash-screen t)        		        ;; Skip splash screen

;; Reduce unnecessary checks
(when (boundp 'use-dialog-box)
  (setq use-dialog-box nil))           		        ;; Use minibuffer instead of dialog boxes
(when (boundp 'use-file-dialog)
  (setq use-file-dialog nil))          		        ;; Use minibuffer for file operations

;; Auto-refresh files and directories (Dired, logs, etc.)
(global-auto-revert-mode 1)
(setq auto-revert-use-notify t)

;; Savehist
(setq history-length 1000)
(setq savehist-autosave-interval 300)

;; Show matching parens
(show-paren-mode 1)
(setq show-paren-delay 0)

;; Always follow symlinks
(setq vc-follow-symlinks t)
(defun my/setup-startup-defaults ()
  "Apply basic startup defaults."
  (setq load-prefer-newer t)
  (setq package-enable-at-startup nil))

;; Buffers
(setq display-buffer-alist
      '((".*" display-buffer-same-window)))

;; Apply performance settings
(my/setup-startup-defaults)

(provide 'defaults)
