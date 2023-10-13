;;;; init.el --- begin the configuration process -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: Jon Boone

;;; Commentary:

;; configure support for custom.el
(setq custom-file (expand-file-name "custom.el"
				    user-emacs-directory))
(when (and custom-file
	   (file-exists-p custom-file))
  (load custom-file nil :nomessage))

;; For now, we will begin by using crafted-init-config module
(load (expand-file-name "modules/crafted-init-config"
			crafted-emacs-home))

;; add package definitions for completion packages
;; to 'package-selected-packages'.
(require 'crafted-completion-packages)

;; install selected packages
(package-install-selected-packages :noconfirm)

;; load configuration for the completion module
(require 'crafted-completion-config)
