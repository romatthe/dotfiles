;;; -*- lexical-binding: t -*-

;; general, a more convenient way to define keybindings
(use-package general
  :straight t)

;; Global key definitions
(general-define-key
 "C-d" 'romatthe/duplicate-line
 "<home>" 'beginning-of-line
 "<end>" 'end-of-line
 ;; Dired keys
 :keymaps 'dired-mode-map
 "c" 'find-file)

;; move-text, provides functions to easily move entire lines up or down
(use-package move-text
  :straight t
  :config
  :bind (("M-S-<up>" . 'move-text-up)
	 ("M-S-<down>" . 'move-text-down)))

(defun romatthe/duplicate-line (arg)
  "Duplicate current line, leaving point in lower line.  ARG is the current line."
  (interactive "*p")
  ;; Save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))
  ;; Local variables for start and end of line
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion
      ;; Don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))
      ;; Store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        ;; Insert the line arg times
        (while (> count 0)
          (newline)         ;; Because there is no newline in 'line'
          (insert line)
          (setq count (1- count))))
      ;; Create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list))))
  ;; Put the point in the lowest line and return
  (next-line arg))
