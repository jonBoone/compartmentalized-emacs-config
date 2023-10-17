;;; custom-writing-config.el --- custom writing configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: Jon Boone

;;; Commentary:


;;; config packages already loaded and partially configed in crafted-writing-config
(with-eval-after-load 'crafted-writing-config
  ;; ensure parens match
  (use-package paren
    :config
    (set-face-attribute 'show-paren-match-expression nil :background "#363e4a"))

  ;; turn on global whitespace instead of tabs
  (with-eval-after-load 'crafted-writing-config
    (crafted-writing-configure-whitespace nil t)))


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
