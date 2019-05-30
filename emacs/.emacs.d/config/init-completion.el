;;; -*- lexical-binding: t -*-

;; company, a code completion framework
(use-package company
  :straight t
  :commands (company-mode
	     global-company-mode
	     company-complete
	     company-complete-common
	     company-manual-begin
	     company-grab-line)
  :hook ((prog-mode . company-mode)
         (comint-mode . company-mode)))

;; company-quickhelp, provides documentation popups through postip
(use-package company-quickhelp
  :straight t
  :after company
  :commands (company-quickhelp-mode)
  :init
  (company-quickhelp-mode 1)
  (use-package pos-tip
    :straight
    :commands (pos-tip-show)))

;; company-box, provides icons in the company drop-down
(use-package company-box
  :disabled
  :straight t
  :after company
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-icons-alist 'company-box-icons-all-the-icons))
