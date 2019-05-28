;;; -*- lexical-binding: t -*-

;; rust-mode, rust-community-maintained mode for writing Rust
(use-package rust-mode
  :straight t
  :after lsp-mode
  :mode ".rs$"
  :config
  (setq rust-format-on-save t))

