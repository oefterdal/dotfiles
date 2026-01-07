(add-to-list 'load-path (expand-file-name "~/.config/emacs/config" user-emacs-directory))

(require 'defaults)

(require 'fonts)
(add-hook 'after-init-hook #'my/setup-fonts)

(require 'packages)

(cond
 ((eq system-type 'darwin) (require 'mac))
 ((eq system-type 'gnu/linux) (require 'linux)))

(require 'ui)         ;; UI Configuration
;;(require 'theme)

;; Keybindings
(require 'keys)

(require 'backups)
