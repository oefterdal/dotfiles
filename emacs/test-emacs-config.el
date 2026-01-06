(setq debug-on-error t)
(setq user-emacs-directory (expand-file-name "~/.emacs.d/"))
(load (expand-file-name "~/.config/emacs/init.el" nil 'nomessage))
(message "Config loaded successfully")