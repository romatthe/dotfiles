;;; -*- lexical-binding: t -*-

;; hydra, make keybindings that stick around
(use-package hydra
  :straight t)

(defhydra hydra-window (:hint nil)
   "
Movement^^    ^Split^
--------------------------------
   ^_↑_^        [_v_]ertical
 _←_   _→_      [_h_]orizontal
   ^_↓_^        [_k_]ill
           ^^^^ kill [_o_]ther

"
   ("<left>" windmove-left)
   ("<down>" windmove-down)
   ("<up>" windmove-up)
   ("<right>" windmove-right)
   ("v" split-window-below)
   ("h" split-window-right)
   ("k" delete-window)
   ("o" delete-other-windows)
   ("S-<left>" romatthe/move-splitter-left)
   ("S-<right>" romatthe/move-splitter-right)
   ("S-<up>" romatthe/move-splitter-up)
   ("S-<down>" romatthe/move-splitter-down)
   ("q" nil "quit"))

(defun romatthe/move-splitter-left (arg)
  "Move window splitter left."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

(defun romatthe/move-splitter-right (arg)
  "Move window splitter right."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

(defun romatthe/move-splitter-up (arg)
  "Move window splitter up."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

(defun romatthe/move-splitter-down (arg)
  "Move window splitter down."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (shrink-window arg)
    (enlarge-window arg)))

(global-set-key (kbd "C-c w") 'hydra-window/body)
