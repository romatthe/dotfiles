(require 'package)

;; Enable MELPA
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t) 
  (when (< emacs-major-version 24)
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; User indentifaction
(setq user-full-name "Robin Mattheussen")
(setq user-mail-address "robin.mattheussen@gmail.com")

;; Clear Window clutter and set up the look and feel
(tool-bar-mode -1)
(menu-bar-mode -1)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)) ;; Change look to match OSX title bar
(add-to-list 'default-frame-alist '(ns-appearance . dark))        ;; Change the titlebar to a dark version
(add-to-list 'default-frame-alist '(fullscreen . maximized))      ;; Start with a maximized frame
;; (set-face-attribute 'default nil :height 130)
(add-to-list 'default-frame-alist
             '(font . "Droid Sans Mono for Powerline-13"))
(setq ns-use-proxy-icon nil)                                      ;; Disable title bar icon

;; Sane defaults
(global-set-key (kbd "<home>") 'beginning-of-line)
(global-set-key (kbd "<end>") 'end-of-line)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-d") 'rm/duplicate-line)
;; (global-set-key (kbd "C-d") 'kill-whole-line)
(delete-selection-mode t)                           ;; Allows overwriting of the selection
(transient-mark-mode t)                             ;; Allows for sane defaults regarding selected region
(show-paren-mode t)                                 ;; Always highlight parens
(setq x-select-enable-clipboard t)                  ;; Interaction with X clipboard
(setq tab-width 4                                   ;; Sane tabs
      indent-tabs-mode nil)

;; Show empty lines
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;; Put backups in one place: flat, no tree structure
(setq backup-directory-alist '(("" . "~/.emacs.d/emacs-backup")))

;; Set auto-paring of braces, quotes, etc.
(electric-pair-mode t)
(setq electric-pair-pairs '((?\" . ?\")
                            (?\{ . ?\})))

;; Configure Use-Package
(eval-when-compile
  (require 'use-package))

;; Install and configure Beacon-Mode
;; Provides easy to follow cursor highlighting
(use-package beacon
  :ensure t
  :config
  (beacon-mode t))

;; Install and configure Org-Bullets for Org-Mode
;; Provides fancy-looking bullets for Org-Mode headings
(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

;; Install and configure nlinum and nlinum-hl
;; Provides line numbers and highlighting
(use-package nlinum
  :ensure t
  :config
  (global-nlinum-mode t))

(use-package nlinum-hl
  :ensure t
  :after nlinum)

;; Install and configure Powerline
;; Provides a fancy and useful bar above the mini-buffer
(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))

;; Install and configure restart-emacs
;; Provides functions to restart Emacs from within Emacs
(use-package restart-emacs
  :ensure t
  :config
  (global-set-key (kbd "C-x C-r") 'restart-emacs))

;; Install and configure Move-Text
;; Provides functions to easily move entire lines up or down
(use-package move-text
  :ensure t
  :config
  (global-set-key (kbd "M-S-<up>") 'move-text-up)
  (global-set-key (kbd "M-S-<down>") 'move-text-down))

;; Install and configure Doom-Themes
;; Provides hlissner's awesome themes from Doom-Emacs
(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

;; Install and configure Projectile
;; Provides project magement functions
(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (projectile-mode t))

;; Install and configure Dashboard
;; Provides a configurable dashboard when starting Emacs
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner "~/.emacs.d/img/ness_transp.png"
	dashboard-items '((recents  . 5)
                          ;;(bookmarks . 5)
                          (projects . 5)
                          (agenda . 5))))
                          ;;(registers . 5))) ;; //TODO

;; Install and configure Which-Key
;; Provides a helpful list of other possible key combinations
(use-package which-key
  :ensure t
  :config
  (which-key-setup-side-window-bottom)
  (which-key-mode))

;; Install and configure Company
;; Provides a general-purpose completion mode with different backends
(use-package company
  :ensure t
  :config
  (global-company-mode t))

;; Install and configure Ivy and Swiper
;; Provides a selection narrowing framework (Ivy) and enhanced search functionality (Swiper)
;; The Swiper package includes Ivy
(use-package swiper
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
	ivy-count-format "%d/%d "
	ivy-case-fold-search t                 ;; Make Swiper search cae-insensitive
	enable-recursive-minibuffers t)
  (global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "C-c C-r") 'ivy-resume))

;; Install and configure Counsel
;; Provides a bunch of useful functions that rely on Ivy
;; Council is part of Ivy
(use-package counsel
  :ensure t
  :after swiper
  :bind (("M-y" . counsel-yank-pop)
	 :map ivy-minibuffer-map
	 ("M-y" . ivy-next-line))
  :config
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

;; Install and configure Counsel-Projectile
;; Provides improved Ivy support for Projectile
(use-package counsel-projectile
  :ensure t
  ;; :after (counsel projectile) //TODO Why won't this work when using after?
  :config
  (counsel-projectile-mode t))

;; Install and configure Web-Mode
;; Provides a very helpful mode for editing (mixed) HTML, CSS, etc. files
(use-package web-mode
  :ensure t
  ;; :hook flycheck-mode //TODO
  :mode (".html?" ".vue?" ".css$")
  :config
  (setq web-mode-style-padding 2
	web-mode-script-padding 2
	web-mode-markup-indent-offset 2
	web-mode-css-indent-offset 2
	web-mode-code-indent-offset 2
	web-mode-enable-current-element-highlight t
	web-mode-enable-auto-closing t
	web-mode-enable-auto-opening t
	web-mode-enable-auto-pairing t
	web-mode-enable-auto-indentation t)
  (add-to-list 'auto-mode-alist '("\\.html$" . web-mode)))

;; Install and configure YAML-Mode
;; Provides major mode for working with YAML files
(use-package yaml-mode
  :ensure t
  :mode (".yaml?" ".yml?"))

;; Install and configure Magit
;; Provides a Git porcelain inside Emacs
(use-package magit
  :ensure t
  :defer t
  :bind (("C-c g" . magit-status)
	 :map magit-status-mode-map
	 ("q" . nm/magit-quit-session)) ;; Extra key to quit the status screen
  :init
  (progn
    (defadvice magit-status (around magit-fullscreen activate)
      "Registers the current window configuration and fullscreens the magit status buffer"
      (window-configuration-to-register :magit-fullscreen)
      ad-do-it
      (delete-other-windows))
    (defadvice magit-status (after magit-show-help activate)
      "Immediately shows the magit help popup after opening the status buffer"
      (magit-dispatch-popup)
      (other-window))
    (defun nm/magit-quit-session ()
      "Kills the magit buffer and restores the previous window configuration"
      (interactive)
      (kill-buffer)
      (jump-to-register :magit-fullscreen))))

;; Install and configure ANSI-Color
;; Provides a hook to use ansi-color on the *compilation* buffer
(use-package ansi-color
  :ensure t
  :hook (compilation-filter . nm/colorize-compilation) 
  :init
  (progn
    (defun nm/colorize-compilation ()
      "Colorize from `compilation-filter-start' to `point'"
      (let ((inhibit-read-only t))
	(ansi-color-apply-on-region
	 compilation-filter-start (point))))))

(defun rm/duplicate-line (arg)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")
  ;; Save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))
  ;; Local variables for start and end of line
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion
      ;; Don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))
      ;; Store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        ;; Insert the line arg times
        (while (> count 0)
          (newline)         ;; Because there is no newline in 'line'
          (insert line)
          (setq count (1- count))))
      ;; Create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list))))
  ;; Put the point in the lowest line and return
  (next-line arg))

;; DONT TOUCH
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (beacon yaml-mode magit counsel-projectile projectile web-mode counsel swiper powerline company org-bullets nlinum-hl restart-emacs move-text which-key use-package doom-themes))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
