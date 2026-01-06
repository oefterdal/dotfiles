;;; -*- lexical-binding: t -*-
;; Font Configuration for Emacs
;; This file handles all font-related settings

(defgroup my-fonts nil
  "Font configuration."
  :group 'faces)

(defcustom my/main-font "JetBrainsMono Nerd Font Mono"
  "Main (monospace) font family."
  :type 'string
  :group 'my-fonts)

(defcustom my/variable-font "JetBrainsMono Nerd Font Propo"
  "Variable-pitch (proportional) font family."
  :type 'string
  :group 'my-fonts)

(defcustom my/font-height 160
  "Default font height (160 = 16pt-ish)."
  :type 'integer
  :group 'my-fonts)

(defun my/font-available-p (name)
  (member name (font-family-list)))

(defun my/setup-fonts ()
  "Apply fonts to current and future frames."
  (interactive)
  (let ((mono (if (my/font-available-p my/main-font) my/main-font "Menlo"))
        (var  (if (my/font-available-p my/variable-font) my/variable-font "Menlo")))

    ;; Default (monospace)
    (set-face-attribute 'default nil :family mono :height my/font-height)

    ;; Variable pitch (Org, text modes, etc.)
    (set-face-attribute 'variable-pitch nil :family var)

    ;; macOS rendering nicety
    (when (eq system-type 'darwin)
      (setq mac-allow-anti-aliasing t))

    ;; Also apply to new frames (daemon / emacsclient)
    (add-to-list 'default-frame-alist `(font . ,(format "%s-%d" mono (/ my/font-height 10)))))

  (message "Fonts set: %s / %s" my/main-font my/variable-font))

;; Call early if you want it immediately on startup:
;; (my/setup-fonts)

(provide 'fonts)
