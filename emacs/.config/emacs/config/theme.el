;; Theme
(setq custom-safe-themes t)
(mapc #'disable-theme custom-enabled-themes)
(when (require 'catppuccin-theme nil t)
(load-theme 'catppuccin t))

(provide 'theme)
