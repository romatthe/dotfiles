;;; -*- lexical-binding: t -*-

;; rust-mode, rust-community-maintained mode for writing Rust
(use-package rust-mode
  :straight t
  :after lsp-mode
  :mode ".rs$"
  :config
  (setq rust-format-on-save t))

;; cargo, provides Cargo commands through minor mode under C-c C-c
(use-package cargo
  :straight t
  :hook (rust-mode . cargo-minor-mode))

;; toml-mode, provides syntax highlighting for TOML files
(use-package toml-mode
  :straight t
  :mode ".toml$")
