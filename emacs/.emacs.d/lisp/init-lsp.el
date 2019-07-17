;;; -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019 Robin Mattheussen

;; Author: Robin Mattheussen <robin.mattheussen@gmail.com>
;; URL: https://github.com/romatthe/dotfiles

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Configuration for LSP mode, for interacting with LSP servers
;;

;;; Code:

(use-package lsp-mode
  :blackout lsp-mode
  :custom
  (lsp-auto-guess-root t)       ; Detect project root
  (lsp-prefer-flymake nil)      ; Use lsp-ui and flycheck
  (flymake-fringe-indicator-position 'right-fringe))

(use-package lsp-ui
  :custom
  (lsp-ui-sideline-enable nil)
  (lsp-ui-doc-enable nil)
  (lsp-ui-flycheck-enable t)
  (lsp-ui-imenu-enable t)
  (lsp-ui-sideline-ignore-duplicate t))

(use-package company-lsp
  :init (setq company-lsp-cache-candidates 'auto))

(use-package lsp-treemacs
  :bind (:map lsp-mode-map
              ("M-9" . lsp-treemacs-errors-list)))


(provide 'init-lsp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp.el ends here
