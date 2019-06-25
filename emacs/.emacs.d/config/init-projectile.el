;;; -*- lexical-binding: t -*-

(use-package projectile
  :straight t
  :init
  (setq projectile-switch-project-action #'projectile-dired
        projectile-completion-system 'ivy)
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (projectile-global-mode 1))
