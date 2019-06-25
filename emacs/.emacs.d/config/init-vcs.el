;;; -*- lexical-binding: t -*-

;; Magit, a git interface
(use-package magit
  :straight t 
  :bind (("C-c g" . magit-status)))

;; diff-hl, shows a git gutter similar to Jetbrains IDEs
(use-package diff-hl
  :straight t
  :hook ((dired-mode . diff-hl-dired-mode)
	 (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (global-diff-hl-mode t)
  (diff-hl-flydiff-mode))
