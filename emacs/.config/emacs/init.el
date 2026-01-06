(add-to-list 'load-path (expand-file-name "~/.config/emacs/config" user-emacs-directory))

;; Load default settings first
(require 'defaults)

;; Load font configuration early (before packages that might depend on fonts)
(require 'fonts)
(my/setup-fonts)

;; Load other modules
(require 'packages)
(require 'ui)
(require 'backups)
(require 'keys)

