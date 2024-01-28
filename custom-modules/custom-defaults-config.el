;;; custom-defaults-config.el --- custom defaults    -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: Jon Boone

;;; Commentary:

;;; Buffers

;; don't warn when reading large files
(setq large-file-warning-threshold nil)

;;; Persistence between sessions

;; configure savehist options
(setq history-length             50
      history-delete-duplicates  t)

;; remember the last cursor location of opened files
(save-place-mode 1)

;; Unset "C-z" (suspend-frame)
(global-unset-key (kbd "C-z"))

;; flyspell package
(use-package flyspell
  :straight t
  :hook (text-mode . flyspell-mode)
  :diminish
  :init
  (add-hook 'prog-mode-hook #'flyspell-prog-mode)
  :config
  (setq ispell-program-name "aspell"
        ispell-extra-args  '("--sug-mode=ultra")))

(use-package default-text-scale
  :defer t
  :config
  (default-text-scale-mode))

;; configure common default settings
(custom-set-default 'tab-width 2)
(custom-set-default 'indent-tabs-mode t)
(custom-set-default 'tabs-always-indent 'complete)

(provide 'custom-defaults-config)
;; END custom-defaults-config.el
;;
