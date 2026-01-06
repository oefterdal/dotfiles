;;; -*- lexical-binding: t -*-
(require 'package)

(setq package-enable-at-startup nil)

(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(unless package--initialized
  (package-initialize))

(unless package-archive-contents
  (message "🔄 Refreshing package archives...")
  (package-refresh-contents)
  (message "✅ Package archives refreshed"))

(defconst my/packages-to-install
  '(use-package
    consult
    vertico
    savehist
    magit
    ace-window
    company
    flycheck
    lsp-mode
    org-roam
    treemacs
    catppuccin-theme)
  "Packages I want installed.")

(defun my/install-all-packages ()
  "Install all packages in `my/packages-to-install`."
  (interactive)
  (dolist (pkg my/packages-to-install)
    (unless (package-installed-p pkg)
      (package-install pkg)))
  (message "✅ Packages installed/verified."))

(defun my/install-package ()
  "Install a single package by name."
  (interactive)
  (let* ((name (read-string "Package name: "))
         (pkg (intern name)))
    (if (package-installed-p pkg)
        (message "✅ %s is already installed" pkg)
      (progn
        (package-install pkg)
        (message "✅ %s installed" pkg)))))

;; Ensure everything is installed at startup (comment out if you prefer manual)
(my/install-all-packages)

;; --- UI completion/navigation ---
(require 'use-package)

(use-package vertico
  :init (vertico-mode 1))

(use-package savehist
  :init (savehist-mode 1))

(use-package consult
  :bind (("C-x b" . consult-buffer)
         ("C-s"   . consult-line)
         ("C-c p s" . consult-ripgrep)))

(provide 'packages)
