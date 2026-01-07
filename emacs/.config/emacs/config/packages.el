;;; -*- lexical-binding: t -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Set up package tls ;;;;

(require 'package)

;; Don't auto-initialize.
(setq package-enable-at-startup nil)

;; Don't add that `custom-set-variables' block to init.
(setq package--init-file-ensured t)

;; From https://irreal.org/blog/?p=8243
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; From https://github.com/hlissner/doom-emacs/blob/5dacbb7cb1c6ac246a9ccd15e6c4290def67757c/core/core-packages.el#L102
(setq gnutls-verify-error (not (getenv "INSECURE")) ; you shouldn't use this
      tls-checktrust gnutls-verify-error
      tls-program (list "gnutls-cli --x509cafile %t -p %p %h"
                        ;; compatibility fallbacks
                        "gnutls-cli -p %p %h"
                        "openssl s_client -connect %h:%p -no_ssl2 -no_ssl3 -ign_eof"))

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("gnu" . "https://elpa.gnu.org/packages/")))

(setq package-archive-priorities
      '(("melpa" .  4)
        ("melpa-stable" . 3)
        ("org" . 2)
        ("gnu" . 1)))

(unless package--initialized
  (package-initialize))

;; Only refresh if needed (still happens on first run)
(unless package-archive-contents
  (package-refresh-contents))

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; use-package-enable-imenu-support must be
;; set before requiring use-package.
(setq use-package-enable-imenu-support t)

(require 'use-package)
(setq use-package-always-ensure t)

;; load path for package modules
(defconst my/config-root
  (expand-file-name "~/.config/emacs/"))

(add-to-list 'load-path
             (expand-file-name "packages" my/config-root))

(require 'pkg-ace-window)         ;;
(require 'pkg-company)            ;;
(require 'pkg-consult)            ;;
(require 'pkg-embark)             ;;
(require 'pkg-flycheck)           ;;
(require 'pkg-lsp-mode)           ;;
(require 'pkg-magit)              ;;
(require 'pkg-marginalia)         ;;
(require 'pkg-markdown)           ;;
(require 'pkg-multiple-cursors)   ;;
(require 'pkg-orderless)          ;;
(require 'pkg-project)            ;;
(require 'pkg-savehist)           ;;
(require 'pkg-saveplace)          ;;
(require 'pkg-ui)                 ;;
(require 'pkg-vertico)            ;;
(require 'pkg-which-key)          ;;
(require 'pkg-windmove)           ;;
(require 'pkg-dired)              ;;
(require 'pkg-yasnippet)          ;;


(provide 'packages)
