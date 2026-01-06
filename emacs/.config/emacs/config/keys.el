;;; -*- lexical-binding: t -*-
(require 'seq)

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'super)
  (setq mac-option-modifier nil)
  (setq mac-right-option-modifier 'meta))

;; Command palette
(global-set-key (kbd "s-p") #'execute-extended-command)

(require 'windmove)
(windmove-default-keybindings)

(global-set-key (kbd "C-c w") #'delete-other-windows)

;; Quick buffer toggle
(global-set-key (kbd "s-b")
                (lambda () (interactive)
                  (switch-to-buffer (other-buffer (current-buffer) 1))))

(with-eval-after-load 'consult
  (global-set-key (kbd "C-x b") #'consult-buffer)
  (global-set-key (kbd "C-s")   #'consult-line)
  (global-set-key (kbd "C-c p s") #'consult-ripgrep))

(with-eval-after-load 'ace-window
  (global-set-key (kbd "M-o") #'ace-window))

(with-eval-after-load 'magit
  (global-set-key (kbd "C-x g") #'magit-status))

;; Formatter
(defun my/format-buffer ()
  (interactive)
  (cond
   ((derived-mode-p 'json-mode)
    (json-pretty-print (point-min) (point-max)))
   ((derived-mode-p 'python-mode)
    (shell-command-on-region (point-min) (point-max) "black -q -" nil t))
   (t
    (indent-region (point-min) (point-max)))))

(global-set-key (kbd "s-S-i") #'my/format-buffer)

(with-eval-after-load 'flycheck
  (add-to-list 'display-buffer-alist
               '("\\*Flycheck errors\\*"
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 0.25)))

  (add-hook 'flycheck-error-list-mode-hook (lambda () (company-mode -1)))

  (defun my/toggle-flycheck-panel ()
    (interactive)
    (let* ((win (seq-find
                 (lambda (w)
                   (string-match-p "\\*Flycheck errors\\*" (buffer-name (window-buffer w))))
                 (window-list)))
           (here (selected-window)))
      (if win
          (quit-window nil win)
        (flycheck-list-errors)
        (select-window here))))

  (global-set-key (kbd "s-S-m") #'my/toggle-flycheck-panel)
  (global-set-key (kbd "C-c ! l") #'my/toggle-flycheck-panel))

(require 'project)
(global-set-key (kbd "C-x p p") #'project-switch-project)
(global-set-key (kbd "C-x p b") #'consult-project-buffer)

(provide 'keys)
