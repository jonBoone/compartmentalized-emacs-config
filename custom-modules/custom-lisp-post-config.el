;;; custom-lisp-post-config.el --- custom lisp post-config -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: Jon Boone

;;; Commentary:


;; add completion-at-point function for language keywords
(add-to-list 'completion-at-point-functions #'cape-symbol)

;; sly post-config
;; keymap bindings
(eval-after-load 'sly
  '(define-key sly-prefix-map (kbd "M-h") 'sly-documentation-lookup))

(eval-after-load 'sly-mrepl
  '(define-key sly-mrepl-mode-map
               (kbd "C-c C-k") 'sly-mrepl-clear-recent-output))


(provide 'custom-lisp-post-config)
;; END custom-lisp-post-config.el
;;

;; custom-lisp-post-config.el ends here
