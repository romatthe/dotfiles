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

(use-package company-quickhelp
  :straight t
  :after company
  :commands (company-quickhelp-mode)
  :init
  (company-quickhelp-mode 1)
  (use-package pos-tip
    :straight
    :commands (pos-tip-show)))

(use-package company-box
  :straight t
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-icons-alist 'company-box-icons-all-the-icons))
