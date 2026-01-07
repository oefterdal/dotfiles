;;; -*- lexical-binding: t; -*-
;; macOS-only config here

(when (memq window-system '(mac ns))

  ;; Better font rendering on macOS
  (setq mac-allow-anti-aliasing t)

  ;; macOS UI / frame behavior
  (setq ns-use-proxy-icon nil
        ns-pop-up-frames nil
        ns-use-native-fullscreen nil)

  ;; Transparent titlebar
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))

  ;; Modifiers: Cmd = Meta, Option = normal symbols
  (setq mac-command-modifier 'meta
        mac-option-modifier 'none
        mac-right-option-modifier 'none)

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
