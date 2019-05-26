;;; -*- lexical-binding: t -*-

;; Mark all themes as safe to load, then set a theme
(setq custom-safe-themes t)
(load-theme 'leuven)

;; Disable the beeping noise on errors
(setq ring-bell-function 'ignore)

;; Disable the default GUI elements
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; Improve mouse/touchpad scrolling
(setq mouse-wheel-scroll-amount '(1)
      mouse-wheel-progressive-speed nil
      scroll-conservatively 101)

;; Disable GTK popups and dialog boxes
(setq x-gtk-system-tooltips nil
      use-dialog-box nil)

;; Disable the blinking cursor
 (blink-cursor-mode 0)

;; Fonts
(if (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(font . "Droid Sans Mono for Powerline-13"))
  (add-to-list 'default-frame-alist '(font . "Hack-10")))

;; Highlight matching parenthesis 
(show-paren-mode t)

;; Highlight current line
(global-hl-line-mode t)

;; This package requires you to run M-x all-the-icons-install-fonts
;; after having installed the package!
(use-package all-the-icons
  :straight t)

;; Enable display of icons in dired
(use-package all-the-icons-dired
  :straight t
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))
