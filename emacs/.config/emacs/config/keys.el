;;; -*- lexical-binding: t -*-
(require 'seq)

;; Command palette
(global-set-key (kbd "s-p") #'execute-extended-command)

(global-set-key (kbd "C-c w") #'delete-other-windows)

;; Quick buffer toggle
(global-set-key (kbd "s-b")
                (lambda () (interactive)
                  (switch-to-buffer (other-buffer (current-buffer) 1))))


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

;; Like RUN in Zed / VSCODE
(global-set-key (kbd "C-c c") #'compile)


(with-eval-after-load 'company
  (define-key company-active-map (kbd "<tab>") #'company-complete-selection)
  (define-key company-active-map (kbd "TAB")   #'company-complete-selection))

(provide 'keys)
