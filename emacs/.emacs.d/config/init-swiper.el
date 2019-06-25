;;; -*- lexical-binding: t -*-

;; ivy, a generic completion mechanics
(use-package ivy
  :straight t
  :hook (after-init . ivy-mode)
  :bind (("C-c C-r" . ivy-resume))
  :config
  (setq ivy-use-virtual-buffers t
	ivy-count-format "%d/%d "
	ivy-case-fold-search t                 ;; Make searches case-insensitive
	enable-recursive-minibuffers t))

;; swiper, search functionality using swiper
(use-package swiper
  :straight t
  :after ivy
  :bind (("\C-s" . swiper)))

;; counsel, additional functionality that uses ivy
(use-package counsel
  :straight t
  :after (ivy swiper)
  :bind (:map ivy-minibuffer-map
	      (("M-y" . ivy-next-line)
	       ("C-r" . counsel-minibuffer-history)))
  :config
  (counsel-mode 1))
