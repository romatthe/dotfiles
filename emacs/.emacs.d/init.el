;;; -*- lexical-binding: t -*-

(defvar startup/file-name-handler-alist file-name-handler-alist)

;; Startup hacks
(setq package-enable-at-startup nil
      file-name-handler-alist nil
      message-log-max 16384
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      auto-window-vscroll nil
      package--init-file-ensured t)

;; Reset startup hacks settings after startup is done
(add-hook 'after-init-hook
          `(lambda ()
             (setq file-name-handler-alist startup/file-name-handler-alist
                   gc-cons-threshold 800000
                   gc-cons-percentage 0.1)) t)

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

(mapc 'load (directory-files (concat (expand-file-name user-emacs-directory) "config") t "^[^#].*el$"))
