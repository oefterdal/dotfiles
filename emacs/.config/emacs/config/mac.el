;;; -*- lexical-binding: t; -*-
;; macOS-only config here

(when (eq system-type 'darwin)

  ;; Better font rendering on macOS
  (setq mac-allow-anti-aliasing t)

  ;; macOS UI / frame behavior
  (setq ns-use-proxy-icon nil
        ns-pop-up-frames nil
        ns-use-native-fullscreen nil)

  ;; Transparent titlebar
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))

  ;; Optimal keymapping for Mac (Norwegian layouts)
  (setq mac-command-modifier 'super
        mac-option-modifier 'none
        mac-right-option-modifier 'meta)

  ;; macOS-style editing keys
  (global-set-key (kbd "s-c") #'kill-ring-save)
  (global-set-key (kbd "s-x") #'kill-region)
  (global-set-key (kbd "s-v") #'yank)
  (global-set-key (kbd "s-a") #'mark-whole-buffer)
  (global-set-key (kbd "s-z") #'undo)
  (global-set-key (kbd "s-s") #'save-buffer)
  (global-set-key (kbd "s-w") #'kill-current-buffer)
  (global-set-key (kbd "s-b") #'consult-project-buffer)
  (global-set-key (kbd "s-p") #'project-find-file)
  (global-set-key (kbd "s-f") #'consult-ripgrep)

(defun my/command-palette-toggle ()
  "Open M-x, or close the minibuffer if it's active."
  (interactive)
  (if (active-minibuffer-window)
      (abort-recursive-edit)
    (call-interactively #'execute-extended-command)))

;; Make M-x start with symbols, not shell commands
(setq extended-command-suggest-shorter t)

;; Keep command palette sorted by relevance
(setq completion-cycle-threshold 3)

;; Global
(global-set-key (kbd "s-P") #'my/command-palette-toggle)

;; Minibuffer (so the same key closes it)
(define-key minibuffer-local-map            (kbd "s-P") #'my/command-palette-toggle)
(define-key minibuffer-local-ns-map         (kbd "s-P") #'my/command-palette-toggle)
(define-key minibuffer-local-completion-map (kbd "s-P") #'my/command-palette-toggle)
(define-key minibuffer-local-must-match-map (kbd "s-P") #'my/command-palette-toggle)
(define-key minibuffer-local-isearch-map    (kbd "s-P") #'my/command-palette-toggle)

  ;; Trash integration
  (setq delete-by-moving-to-trash t)

  (when (executable-find "trash")
    (defun system-move-file-to-trash (file)
      "Move FILE to macOS Trash using the `trash` CLI."
      (call-process "trash" nil 0 nil "-F" file)))

  ;; Keyboard helpers
  (defun me/macos-insert-hash ()
    "Insert # (useful on some layouts where M-3 isn't #)."
    (interactive)
    (insert "#"))
  (global-set-key (kbd "M-3") #'me/macos-insert-hash)

  ;; Re-apply fonts on new frames (daemon / emacsclient)
  (add-hook 'after-make-frame-functions
            (lambda (_f) (my/setup-fonts))))

(provide 'mac)
