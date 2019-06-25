;;; -*- lexical-binding: t -*-

;; scala-mode, scala-community maintained for writing Scala
(use-package scala-mode
  :straight t
  :after lsp-mode
  :mode "\\.s\\(cala\\|bt\\)$")

;; sbt-mode, minor mode for interacting with the Scala build tool
(use-package sbt-mode
  :straight t
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))
