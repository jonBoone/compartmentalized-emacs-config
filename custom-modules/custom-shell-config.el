;;; custom-shell-config.el --- custom shell configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: Jon Boone

;;; Commentary:

(defun my/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)
  ;; Truncate buffer for improved performance
  (add-to-list 'eshell-output-filter-functions eshell-truncate-buffer')
  (setq eshell-history-size               10000
        eshell-buffer-maximum-lines       10000
        eshell-hist-ignoredups            t
        eshell-scroll-to-bottom-on-input  t))

(use-package eshell-git-prompt
  :after eshell)

(use-package eshell
  :hook (eshell-first-time-mode . my/configure-eshell)
  :config
  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t
          eshell-visual-commands                  '("htop" "zsh" "vim")))
  (eshell-git-prompt-use-theme 'powerline))

(provide 'custom-shell-config)
;; END custom-shell-config.el
;;

;; custom-shell-config.el ends here
