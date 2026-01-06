(setq debug-on-error t)
(setq user-emacs-directory (expand-file-name "~/.emacs.d/"))

;; Test loading each file individually
(message "Loading core.el...")
(load (expand-file-name "~/.config/emacs/config/core.el") nil t)
(message "core.el loaded successfully")

(message "Loading packages.el...")
(load (expand-file-name "~/.config/emacs/config/packages.el") nil t)
(message "packages.el loaded successfully")

(message "Loading fonts.el...")
(load (expand-file-name "~/.config/emacs/config/fonts.el") nil t)
(message "fonts.el loaded successfully")

(message "Loading ui.el...")
(load (expand-file-name "~/.config/emacs/config/ui.el") nil t)
(message "ui.el loaded successfully")

(message "All configuration files loaded successfully!")