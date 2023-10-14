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

;;;; modules

;;; leverage my-start-config
(require 'my-start-config)

;;; leverage my-font-config
(require 'my-font-config)

;; additional elisp libraries
(require 'additional-elisp-libraries-packages)
(crafted-package-install-selected-packages)

;;; leverage performance-optimization module
(require 'optimized-performance-packages)
(crafted-package-install-selected-packages)
(require 'optimized-performance-config)

;;; crafted-init-config
(require 'crafted-init-config)

;; keybinding libraries module
(require 'keybinding-libraries-packages)
(crafted-package-install-selected-packages)
(require 'keybinding-libraries-config)

;;; crafted-defaults - the General Crafted Emacs endorsed defaults
(require 'crafted-defaults-config)

;;; crafted-ui module
(require 'crafted-ui-packages)
(crafted-package-install-selected-packages)
(require 'crafted-ui-config)

;;; crafted-completion module
(require 'crafted-completion-packages)
(crafted-package-install-selected-packages)
(require 'crafted-completion-config)

;;; crafted-idea module
(require 'crafted-ide-packages)
(crafted-package-install-selected-packages)
(require 'crafted-ide-config)

;;; crafted-lisp module
(require 'crafted-lisp-packages)
(crafted-package-install-selected-packages)
(require 'crafted-lisp-config)

(provide 'init)

;; END init.el
;;

;; init.el ends here
