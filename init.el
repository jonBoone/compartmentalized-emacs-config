;;;; init.el --- begin the configuration process -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: Jon Boone

;;; Commentary:

;;; custom.el support
(setq custom-file (expand-file-name "custom.el"
				    user-emacs-directory))
(when (and custom-file
	   (file-exists-p custom-file))
  (load custom-file nil :nomessage))

;;; crafted-emacs modules

;;; crafted-init-config
(require 'crafted-init-config)
;(load (expand-file-name "modules/crafted-init-config"
;			crafted-emacs-home))

;;; crafted-defaults - the General Crafted Emacs endorsed defaults
(require 'crafted-defaults-config)

;;; crafted-ui module
(require 'crafted-ui-packages)
(package-install-selected-packages :noconfirm)
(require 'crafted-ui-config)

;;; crafted-completion module
(require 'crafted-completion-packages)
(package-install-selected-packages :noconfirm)
(require 'crafted-completion-config)

;;; crafted-idea module
(require 'crafted-ide-packages)
(package-install-selected-packages :noconfirm)
(require 'crafted-ide-config)

;;; crafted-lisp module
(require 'crafted-lisp-packages)
(package-install-selected-packages :noconfirm)
(require 'crafted-lisp-config)

(provide 'init)

;; END init.el
;;

;; init.el ends here
