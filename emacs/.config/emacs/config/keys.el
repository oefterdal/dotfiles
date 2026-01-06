;;; -*- lexical-binding: t -*-

;; macOS modifiers
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'super)
  (setq mac-right-option-modifier 'none))

;; Windmove (built-in)
(require 'windmove)
(windmove-default-keybindings) ;; Shift+arrow to move between windows

;; Swap windows (built-in in newer Emacs; if not available you’ll notice)
(when (fboundp 'windmove-swap-states-left)
  (global-set-key (kbd "M-S-<left>")  #'windmove-swap-states-left)
  (global-set-key (kbd "M-S-<right>") #'windmove-swap-states-right)
  (global-set-key (kbd "M-S-<up>")    #'windmove-swap-states-up)
  (global-set-key (kbd "M-S-<down>")  #'windmove-swap-states-down))

;; Ace Window (lazy-load)
(global-set-key
 (kbd "M-o")
 (lambda () (interactive)
   (require 'ace-window)
   (ace-window)))

;; Layout reset
(global-set-key (kbd "C-c w") #'delete-other-windows)

;; Font size
(global-set-key (kbd "C--") #'text-scale-decrease)
(global-set-key (kbd "C-0") #'text-scale-adjust)
(global-set-key (kbd "C-=") #'text-scale-increase) ;; common on mac
(global-set-key (kbd "C-+") #'text-scale-increase) ;; some keyboards send this

;; Package shortcuts (lazy-load packages.el)
(global-set-key
 (kbd "C-c C-p a")
 (lambda () (interactive)
   (require 'packages)
   (my/install-all-packages)))

(global-set-key
 (kbd "C-c C-p i")
 (lambda () (interactive)
   (require 'packages)
   (my/install-package)))

(global-set-key (kbd "C-c C-x r") #'package-refresh-contents)

;; Quick switch to previous buffer
(global-set-key (kbd "s-b")
                (lambda () (interactive)
                  (switch-to-buffer (other-buffer (current-buffer) 1))))

;; Keybinding diagnostics (only if defined)
(global-set-key
 (kbd "C-c C-k d")
 (lambda () (interactive)
   (if (fboundp 'my/diagnose-keybindings)
       (my/diagnose-keybindings)
     (message "my/diagnose-keybindings is not defined"))))

(provide 'keys)
