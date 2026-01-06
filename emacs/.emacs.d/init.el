;; Keep userdata here
(setq user-emacs-directory (expand-file-name "~/.emacs.d/"))

;; Make Custom write here (not into init.el)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file 'noerror 'nomessage))

;; Load real config from ~/.config/emacs
(load (expand-file-name "~/.config/emacs/init.el") nil 'nomessage)
