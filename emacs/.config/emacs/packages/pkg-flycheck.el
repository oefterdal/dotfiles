;;; -*- lexical-binding: t -*-

(use-package flycheck
  :hook (after-init . global-flycheck-mode)
  :init
  ;; Show errors in minibuffer unless the error list is open
  (setq flycheck-display-errors-function
        #'flycheck-display-error-messages-unless-error-list)

  :config
  ;; Show Flycheck error list as a bottom "panel"
  (add-to-list 'display-buffer-alist
               '("\\*Flycheck errors\\*"
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 0.25)
                 (window-parameters . ((no-delete-other-windows . t)))))

  (defun my/toggle-flycheck-panel ()
    "Toggle Flycheck errors panel without stealing focus."
    (interactive)
    (let* ((buf (get-buffer "*Flycheck errors*"))
           (win (and buf (get-buffer-window buf t)))
           (here (selected-window)))
      (if win
          (quit-window nil win)
        (flycheck-list-errors)
        (select-window here))))

  ;; Default bindings (you can override in keys.el)
  (global-set-key (kbd "C-c ! l") #'my/toggle-flycheck-panel)
  (global-set-key (kbd "s-S-m")   #'my/toggle-flycheck-panel))

(provide 'pkg-flycheck)
