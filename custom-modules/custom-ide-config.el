;;; custom-ide-config.el --- custom ide configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: Jon Boone

;;; Commentary:

;; setup compiler command support
(use-package compile
  :straight t
  :defer t
  :hook
  ((c++-mode c-mode java-mode javascript-mode go-mode nroff-mode) . generic-compiler)
  :bind
  (("C-x M-m" . compile)
   ("C-x C-m" . recompile))

  :init
  (defun has-makefile-p ()
    (or (file-exists-p "makefile")
        (file-exists-p "Makefile")))

  (defun generic-compiler ()
    (unless (has-makefile-p)
      (setq-local compile-command
                  (concat "compiler "
                          (when buffer-file-name
                            (shell-quote-argument buffer-file-name))))))
  :config
  (setq compilation-scroll-output t)
  (require 'ansi-color)
  (defun colorize-compilation-buffer ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max))))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))

;; ensure we have more space when editing a parent-sexp
(use-package expand-region
  :straight t
  :bind
  (("M-[" . er/expand-region)
   ("C-(" . er/mark-outside-pairs)))

;; colorize nested parentheses and brackets according to their nesting depth
(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

(provide 'custom-ide-config)
;; END custom-ide-config.el
;;

;; custom-ide-config.el ends here
