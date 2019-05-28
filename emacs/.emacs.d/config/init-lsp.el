;;; -*- lexical-binding: t -*-

;; lsp-mode, general purpose mode for talking to LSP servers
(use-package lsp-mode
  :straight t
  :defer t
  :commands lsp
  :init
  (setq lsp-auto-guess-root t))

;; lsp-ui, mode for providing visual feedback from LSP servers
(use-package lsp-ui
  :straight t
  :defer t
  :after lsp-mode
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui)
  :bind (:map lsp-ui-mode-map
	      ([remap xref-find-references] . lsp-ui-peek-find-references))
  :init
  (setq lsp-ui-peek-always-show t
	lsp-ui-sideline-enable t
	lsp-ui-doc-enable t
	lsp-ui-doc-use-webkit t))

;; company-lsp, provides autocompletion through Company from LSP servers
(use-package company-lsp
  :straight t
  :after (company lsp-mode)
  :commands company-lsp
  :bind (:map rust-mode-map
	      ("TAB" . company-indent-or-complete-common))
  :config
  (push 'company-lsp company-backends))
