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

;; leverage the General Crafted Emacs endorsed defaults
(require 'crafted-defaults-config)


;; add package definitions for completion packages
(require 'crafted-completion-packages)

;; install selected packages
(package-install-selected-packages :noconfirm)

;; load configuration for the completion module
(require 'crafted-completion-config)

;; add package definitions for ide packages
(require 'crafted-ide-packages)

;; install selected packages
(package-install-selected-packages :noconfirm)

;; load configuration for the ide module
(require 'crafted-ide-config)
