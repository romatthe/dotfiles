;;; -*- lexical-binding: t -*-

(with-eval-after-load 'dired 
  (define-key dired-mode-map "c" 'find-file))
