;;; custom-term-config.el --- custom term configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Termntifier: MIT

;; Author: Jon Boone

;;; Commentary:

;; set up term package
(use-package term
  :commands term
  :config
   ; term-prompt-regexp       "^[^#$%>\n]*[#$%>] *" -- update with zsh prompt regex
  (setq explicit-shell-file-name "zsh"))

;; get better color support for term-mode
(use-package eterm-256color
  :hook term-mode)

;; vterm is my preferred terminal mode
(use-package vterm
  :straight t
  :init
  (setq vterm-always-compile-module t)
  :bind
  (("C-x t" . vterm)
   :map vterm-mode-map
   ("M-p" . vterm-send-up)
   ("M-n" . vterm-send-down))
  :commands vterm
  :config
  (setq vterm-shell           "zsh"
        vterm-max-scrollback  10000))

;; multi-vterm mode allows multiple vterms simultaneously
(use-package multi-vterm
  :straight t
  :after vterm
  :config
  (setq multi-vterm-dedicated-window-height-percent 10))

(provide 'custom-term-config)
;; END custom-term-config.el
;;

;; custom-term-config.el ends here
