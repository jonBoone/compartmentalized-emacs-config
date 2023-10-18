;;; custom-writing-config.el --- custom writing configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: Jon Boone

;;; Commentary:

;; ensure parens match
(when (require 'paren nil :noerror)
  (set-face-attribute 'show-paren-match-expression nil :background "#363e4a"))

;; turn on global whitespace instead of tabs
(when (require 'crafted-writing-config nil :noerror)
  (crafted-writing-configure-whitespace nil t))

;; set the default tab-width to 2 spaces
(custom-set-default 'tab-width 2)

;; automatically cleanup whitespace
(use-package ws-butler
  :straight t
  :hook (prog-mode text-mode))

(provide 'custom-writing-config)
;; END custom-writing-config.el
;;

;; custom-writing-config.el ends here