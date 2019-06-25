;;; -*- lexical-binding: t -*-

;; flycheck, an error checking framework
(use-package flycheck
  :straight t
  :init
  ;; Draw a custom bitmap for the indicators, more in line with Jetbrains IDEs
  (define-fringe-bitmap 'flycheck-fringe-indicator
    (vector #b0000000000000000
	    #b0000000000000000
	    #b0000000000000000
	    #b0000000000000000
	    #b0000000000000000
	    #b1111111111111111
	    #b1111111111111111
	    #b1111111111111111
	    #b1111111111111111
	    #b1111111111111111
	    #b1111111111111111
	    #b0000000000000000
	    #b0000000000000000
	    #b0000000000000000
	    #b0000000000000000
	    #b0000000000000000
	    #b0000000000000000)
    nil 16)
  ;; Draw the indicators on the right side, also in line with Jetbrains IDEs
  :custom (flycheck-indication-mode 'right-fringe)
  ;; Using global mode is the recommended way to use flycheck
  :hook ((after-init . global-flycheck-mode))
  :config
  (flycheck-define-error-level 'error
			       :severity 2
			       :overlay-category 'flycheck-error-overlay
			       :fringe-bitmap 'flycheck-fringe-indicator
			       :fringe-face 'flycheck-fringe-error)
  (flycheck-define-error-level 'warning
			       :severity 1
			       :overlay-category 'flycheck-warning-overlay
			       :fringe-bitmap 'flycheck-fringe-indicator
			       :fringe-face 'flycheck-fringe-warning)
  (flycheck-define-error-level 'info
			       :severity 0
			       :overlay-category 'flycheck-info-overlay
			       :fringe-bitmap 'flycheck-fringe-indicator
			       :fringe-face 'flycheck-fringe-info))

;; flycheck-pos-tip, displays errors in a pos-tip tooltip
(use-package flycheck-pos-tip
  :straight t
  :after flycheck
  :hook (flycheck-mode . flycheck-pos-tip-mode))
