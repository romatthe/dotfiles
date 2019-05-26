;;; -*- lexical-binding: t -*-

;; Easy confirmation
(defalias 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-process nil)

;; Security settings
(setq gnutls-verify-error t
      tls-checktrust t)

;; Save and restore session when restarting
(desktop-save-mode t)
(save-place-mode t)

;; Immediately display keystrokes in the minibuffer
(setq echo-keystrokes 0.1)

;; which-key, shows a popup for possible key combinations
(use-package which-key
  :straight t
  :init (which-key-mode))

;; smartparens, smart editing with parenthesis
(use-package smartparens
  :straight t
  :config
  ;; Smartparens doesn't use autoloads so we still need package.el
  (use-package smartparens-config :straight nil)
  (smartparens-global-mode))
