;;; custom-lisp-config.el --- custom ide configuration  -*- lexical-binding: t; -*-

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


(provide 'custom-lisp-config)
;; END custom-lisp-config.el
;;

;; custom-lisp-config.el ends here
