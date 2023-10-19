;;; custom-ide-config.el --- custom ide configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: Jon Boone

;;; Commentary:

;; setup magit-related version control support
(use-package magit
  :straight t
  :bind
  (("C-M-;" . magit-status)
   :map project-prefix-map
   ("m" . project-magit))
  :commands
  (magit-status magit-get-current-branch project-magit)
  :config
  (add-to-list 'project-switch-commands
               '(project-magit "Magit" m))
  (defun project-magit ()
    (interactive)
    (let ((dir (project-root (project-current t))))
      (magit-status dir)))
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge
  :straight t
  :after magit)

(use-package magit-todos
  :straight t
  :defer t)

(use-package git-link
  :commands git-link
  :config
  (setq git-link-open-in-browser-t))

(use-package ediff
  :after (magit vc)
  :commands ediff
  :config
  (with-eval-after-load 'winner
    (add-hook 'ediff-quit-hook 'winner-undo))
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package diff-hl
  :straight t
  :defer 5
  :hook
  ((magit-pre-refresh . diff-hl-magit-pre-refresh)
   (magit-pre-refresh . diff-hl-magit-post-refresh))
  :init
  (global-diff-hl-mode)
  :config
  (diff-hl-flydiff-mode))

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
