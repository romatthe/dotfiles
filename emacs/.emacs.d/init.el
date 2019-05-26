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

;; Fonts
(if (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(font . "Droid Sans Mono for Powerline-13"))
  (add-to-list 'default-frame-alist '(font . "Hack-10")))

;; Easy confirmation
(defalias 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-process nil)

;; Highlight matching parenthesis 
(show-paren-mode t)

;; Highlight current line
(global-hl-line-mode t)

;; Security settings
(setq gnutls-verify-error t
      tls-checktrust t)

;; Save and restore session when restarting
(desktop-save-mode t)
(save-place-mode t)

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Use use-package together with straight.el
(straight-use-package 'use-package)

(setq dotfile-dir (file-name-directory
                   (file-chase-links
                    (or load-file-name
                        (buffer-file-name))))
      dotfile-dir-config (concat dotfile-dir "config/"))

;; (use-package init-common :load-path "config/")

(mapc 'load (directory-files (concat (expand-file-name user-emacs-directory) "config") t "^[^#].*el$"))

;; Add modular configuration files to the load path
;;(load (concat dotfile-dir-config "init-common.el"))
;;(load (concat dotfile-dir-config "init-visual.el"))

;;(directory-files dotfile-dir-config t directory-files-no-dot-files-regexp)

;;(dolist
;;    (file (directory-files dotfile-dir-config t directory-files-no-dot-files-regexp))
;;  (if (string= (file-name-extension file) "el")
;;      (print (file-name-extension file))))
