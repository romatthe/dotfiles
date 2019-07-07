;;; -*- lexical-binding: t -*-

;; Easy confirmation
(defalias 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-process nil)

;; Security settings
(setq gnutls-verify-error t
      tls-checktrust t)

;; Save and restore session when restarting
;;(desktop-save-mode t)
;;(save-place-mode t)

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

;; Sane defaults for backups and autosaves
(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
      delete-old-versions -1
      version-control t
      vc-make-backup-files t
      auto-save-list-file-prefix "~/.emacs.d/autosave/"
      auto-save-file-name-transforms '((".*" "~/.emacs.d/autosave/" t)))

;; Sane defaults for history
(setq savehist-file "~/.emacs.d/savehist"
      history-length t
      history-delete-duplicates t
      savehist-save-minibuffer-history 1
      savehist-additional-variables '(kill-ring search-ring regexp-search-ring)
      recentf-max-saved-items 50)
(savehist-mode 1)
(recentf-mode 1)

;; exec-path-from-shell, allows correct handling of $PATH from Emacs
(use-package exec-path-from-shell
  :straight t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; restarts-emacs, a way to quickly restart emacs
(use-package restart-emacs
  :straight t
  :bind ("C-x C-r" . restart-emacs))

;; dashboard, a configurable dashboard when starting Emacs
(use-package dashboard
  :straight (dashboard :type git :host github :repo "emacs-dashboard/emacs-dashboard"
                       :fork (:host github
				    :repo "romatthe/emacs-dashboard"))
  :config
  (setq dashboard-startup-banner "~/.emacs.d/img/ness_hamburger_circle.png"
	dashboard-items '((recents  . 5)
                          (projects . 5)
			  (agenda . 5)))
  (dashboard-setup-startup-hook))
