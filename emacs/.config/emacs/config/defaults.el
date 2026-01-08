;;; -*- lexical-binding: t; -*-

;; Startup
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t
      inhibit-splash-screen t)

(when (boundp 'use-dialog-box) (setq use-dialog-box nil))
(when (boundp 'use-file-dialog) (setq use-file-dialog nil))

(setq load-prefer-newer t
      package-enable-at-startup nil)

;; Auto-refresh
(global-auto-revert-mode 1)
(setq auto-revert-use-notify t)

;; History
(setq history-length 1000
      savehist-autosave-interval 300)

;; Parens
(show-paren-mode 1)
(setq show-paren-delay 0)

;; Git/symlinks
(setq vc-follow-symlinks t)

;; Start with blank scratch + open dired in ~/Developer
(setq initial-scratch-message ""
      initial-buffer-choice "~/Developer")

;; TAB: indent, but if already indented, try completion
(setq tab-always-indent 'complete)

;; No tabs in files
(setq-default indent-tabs-mode nil)

(defun my/tab-dwim (&optional arg)
  "Indent region/line; if nothing changed, try completion."
  (interactive "P")
  (cond
   ((use-region-p)
    (indent-region (region-beginning) (region-end)))

   (t
    (let ((before (buffer-substring-no-properties
                   (line-beginning-position) (line-end-position))))
      (indent-for-tab-command arg)
      (let ((after (buffer-substring-no-properties
                    (line-beginning-position) (line-end-position))))
        (when (string= before after)
          (completion-at-point)))))))

;; Bind TAB everywhere (TAB == C-i)
(global-set-key (kbd "TAB")   #'my/tab-dwim)
(global-set-key (kbd "<tab>") #'my/tab-dwim)
(global-set-key (kbd "C-i")   #'my/tab-dwim)

;; Force completion explicitly (nice to have)
(global-set-key (kbd "M-TAB") #'completion-at-point)
(global-set-key (kbd "C-M-i") #'completion-at-point)

(provide 'defaults)
