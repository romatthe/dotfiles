;; This package requires you to run M-x all-the-icons-install-fonts
;; after having installed the package!
(use-package all-the-icons
  :straight t)

;; Enable display of icons in dired
(use-package all-the-icons-dired
  :straight t
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))
