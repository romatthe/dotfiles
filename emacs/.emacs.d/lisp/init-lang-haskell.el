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
;; Configuration for working with the Haskell language
;;

;;; Code:

(use-package haskell-mode
  :bind (:map haskell-mode-map
              ;; this is set to use cabal for dante users and stack for intero users:
              ("b" . haskell-process-cabal-build)
              ("c" . haskell-cabal-visit-file)
              ("h" . haskell-hide-toggle)
              ("H" . haskell-hide-toggle-all))
  :hook ((haskell-mode-hook . haskell-collapse-mode)
         (haskell-mode-hook . interactive-haskell-mode))
  :custom
  (haskell-process-suggest-remove-import-lines t)
  (haskell-process-auto-import-loaded-modules t)
  (haskell-process-show-overlays nil) ; redundant with flycheck
  :config
  (add-to-list 'completion-ignored-extensions ".hi"))

(use-package lsp-haskell
  :after haskell-mode
  :hook (haskell-mode . lsp))


(provide 'init-lang-haskell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lang-haskell.el ends here
