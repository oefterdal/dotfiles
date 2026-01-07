;; Platform-specific optimizations
(when (eq system-type 'darwin)
  (setq ns-use-native-fullscreen nil)  		;; Better fullscreen behavior
  (setq ns-pop-up-frames nil))          		;; Prevent pop-up frames

(when (eq system-type 'gnu/linux)
  (when (boundp 'x-gtk-use-system-tooltip)
    (setq x-gtk-use-system-tooltip nil))  	;; Faster tooltips
  (when (boundp 'x-gtk-file-dialogs)
    (setq x-gtk-file-dialogs nil)))        	;; Disable GTK file dialogs

;; Startup Performance
(setq inhibit-startup-message t)      		;; Skip startup message
(setq inhibit-startup-echo-area-message t)  	;; Skip echo area messages
(setq inhibit-splash-screen t)        		;; Skip splash screen

;; Reduce unnecessary checks
(when (boundp 'use-dialog-box)
  (setq use-dialog-box nil))           		;; Use minibuffer instead of dialog boxes
(when (boundp 'use-file-dialog)
  (setq use-file-dialog nil))          		;; Use minibuffer for file operations

;; Save place between sessions
(require 'saveplace)
(save-place-mode 1)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; Auto-refresh files and directories (Dired, logs, etc.)
(global-auto-revert-mode 1)
(setq auto-revert-use-notify t)

;; Savehist
(setq history-length 1000)
(setq savehist-autosave-interval 300)

;; Show matching parens 
(show-paren-mode 1)
(setq show-paren-delay 0)

;; Project
(require 'project)

;; Which key
(setq which-key-idle-delay 0.3)

;; Flycheck
(with-eval-after-load 'flycheck
  (add-to-list 'display-buffer-alist
               '("\\*Flycheck errors\\*"
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 0.25)
                 (window-parameters . ((no-delete-other-windows . t))))))

;; Always follow symlinks
(setq vc-follow-symlinks t)
(defun my/setup-startup-defaults ()
  "Apply basic startup defaults."
  (setq load-prefer-newer t)
  (setq package-enable-at-startup nil)) 

;; Native Compilation (Emacs 29+)
(when (and (fboundp 'native-comp-available-p) (native-comp-available-p))
  (when (boundp 'native-comp-speed)
    (setq native-comp-speed 2))          ;; 0=slowest, 2=fast, 3=fastest

  (when (boundp 'comp-deferred-compilation)
    (setq comp-deferred-compilation t))  ;; Defer compilation (smoother startup)

  (when (boundp 'comp-speed)
    (setq comp-speed 2))                 ;; Compilation speed

  (when (boundp 'comp-async-report-warnings-errors)
    (setq comp-async-report-warnings-errors t)) ;; Show warnings/errors

  (message "⚡ Native compilation enabled"))

;; Buffers
(setq display-buffer-alist
      '((".*" display-buffer-same-window)))

;; Apply performance settings
(my/setup-startup-defaults)

(provide 'defaults)
