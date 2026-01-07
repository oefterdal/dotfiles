;;; -*- lexical-binding: t; -*-
;; Linux-only config here

(when (string-equal system-type "gnu/linux")
  (setq exec-path (append exec-path '("~/local/bin"))))

(provide 'linux)
