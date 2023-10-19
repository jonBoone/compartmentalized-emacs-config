;;; custom-lisp-post-config.el --- custom lisp post-config -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: Jon Boone

;;; Commentary:

;; use lispy package
(use-package lispy
  :straight t
  :hook (emacs-lisp-mode lisp-mode scheme-mode))

;; use parinfer for lisp languages
(use-package parinfer
  :hook
  (clojure-mode emacs-lisp-mode common-lisp-mode scheme-mode lisp-mode)
  :config
  (setq parinfer-extensions
        '(defaults      ; should be included
          pretty-parens ; different paren styles for different modes
          smart-tab     ; C-b & C-f jump positions and smart shift with tab & S-tab
          smart-yank))) ; Yank behavior depends on mode

;; use smartparens
(use-package smartparens
  :straight t
  :hook prog-mode)

;; sly post-config

;; keymap bindings
(eval-after-load 'sly
  `(define-key sly-prefix-map (kbd "M-h") 'sly-documentation-lookup))

(eval-after-load 'sly-mrepl
  `(define-key sly-mrepl-mode-map (kbd "C-c C-k") `sly-mrepl-clear-recent-output)
  (custom-set-faces 'sly-mrepl-output-face '((t (:foreground "sienna")))))


(provide 'custom-lisp-post-config)
;; END custom-lisp-post-config.el
;;

;; custom-lisp-post-config.el ends here
