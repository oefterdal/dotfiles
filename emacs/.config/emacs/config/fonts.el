;;; -*- lexical-binding: t -*-
;; Font Configuration for Emacs

(defgroup my-fonts nil
  "Font configuration."
  :group 'faces)

(defcustom my/main-font "JetBrains Mono"
  "Main (monospace) font family."
  :type 'string
  :group 'my-fonts)

(defcustom my/variable-font "JetBrains Mono"
  "Variable-pitch (proportional) font family."
  :type 'string
  :group 'my-fonts)

(defcustom my/font-height 160
  "Default font height (160 = 16pt-ish)."
  :type 'integer
  :group 'my-fonts)

(defun my/font-available-p (name)
  "Return non-nil if font family NAME is available."
  (member name (font-family-list)))

(defun my/setup-fonts ()
  "Apply fonts to current frame and set defaults for future frames."
  (interactive)
  (let* ((mono (if (my/font-available-p my/main-font) my/main-font "Menlo"))
         (var  (if (my/font-available-p my/variable-font) my/variable-font "Menlo")))

    ;; Current frame faces
    (set-face-attribute 'default nil :family mono :height my/font-height)
    (set-face-attribute 'variable-pitch nil :family var :height my/font-height)

    ;; Future frames (daemon / emacsclient)
    (setf (alist-get 'font default-frame-alist)
          (format "%s-%d" mono (/ my/font-height 10)))

    (message "Fonts set: %s / %s" mono var)))

(provide 'fonts)
