;;; -*- lexical-binding: t -*-

;; lsp-mode, general purpose mode for talking to LSP servers
(use-package lsp-mode
  :straight t
  :defer t
  :commands lsp
  :init
  (setq lsp-auto-guess-root t
	lsp-enable-snippet nil)) ;; Requires installing Yasnappit

;; lsp-ui, mode for providing visual feedback from LSP servers
(use-package lsp-ui
  :straight t
  :defer t
  :after lsp-mode
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :bind (:map lsp-ui-mode-map
	      ([remap xref-find-references] . lsp-ui-peek-find-references))
  :init
  (setq lsp-ui-peek-always-show t
	lsp-ui-sideline-enable t
	lsp-ui-doc-enable t
	lsp-ui-doc-use-webkit t
	lsp-ui-flycheck-enable t))

;; company-lsp, provides autocompletion through Company from LSP servers
(use-package company-lsp
  :straight t
  :after (company lsp-mode)
  :commands company-lsp
  :bind (:map rust-mode-map
	      ("C-SPC" . company-indent-or-complete-common))
  :config
  (push 'company-lsp company-backends)
  ;; Disable client-side cache because the LSP server does a better job.
  (setq company-transformers nil
        company-lsp-async t
        company-lsp-cache-candidates nil))
