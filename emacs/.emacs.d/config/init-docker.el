;;; -*- lexical-binding: t -*-

;; dockerfile, provides support for Dockerfile syntax
(use-package dockerfile-mode
  :straight t
  :mode "Dockerfile\\'")

(use-package docker
  :straight t
  :bind ("C-c d" . docker))
