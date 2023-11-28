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

;;; leverage my-start module
(require 'my-start-packages)
(crafted-package-install-selected-packages)
(require 'my-start-config)

;;; leverage my-font module
(require 'my-font-packages)
(crafted-package-install-selected-packages)
(require 'my-font-config)

;; additional elisp libraries
(require 'additional-elisp-libraries-packages)
(crafted-package-install-selected-packages)
(require 'additional-elisp-libraries-config)

;;; leverage performance-optimization module
(require 'optimized-performance-packages)
(crafted-package-install-selected-packages)
(require 'optimized-performance-config)

;;; crafted-init-config
(require 'crafted-init-config)

;; workspace configurations
(require 'custom-workspaces-packages)
(package-install-selected-packages)
(require 'custom-workspaces-config)


;; keybinding libraries module
(require 'keybinding-libraries-packages)
(crafted-package-install-selected-packages)
(require 'keybinding-libraries-config)

;;; leverage custom-defaults module
(require 'custom-defaults-packages)
(crafted-package-install-selected-packages)
(require 'custom-defaults-config)

;;; add support for org-mode
(require 'custom-org-packages)
(crafted-package-install-selected-packages)
(require 'custom-org-config)

;;; crafted-defaults - the General Crafted Emacs endorsed defaults
(require 'crafted-defaults-config)

;;; leverage my-files module
(require 'my-files-packages)
(crafted-package-install-selected-packages)
(require 'my-files-config)

;;; custom-ui-mode
(require 'custom-ui-packages)
(crafted-package-install-selected-packages)
(require 'custom-ui-config)

;;; crafted-completion module
(require 'crafted-completion-packages)
(crafted-package-install-selected-packages)
(require 'crafted-completion-config)

;;; custom-term module
(require 'custom-term-packages)
(crafted-package-install-selected-packages)
(require 'custom-term-config)

;;; custom-gnus module
(require 'custom-gnus-packages)
(crafted-package-install-selected-packages)
(require 'custom-gnus-config)

;;; crafted-writing module
(require 'crafted-writing-packages)
(crafted-package-install-selected-packages)
(require 'crafted-writing-config)

;;; custom-writing module
(require 'custom-writing-packages)
(crafted-package-install-selected-packages)
(require 'custom-writing-config)

;;; crafted-ide module
(require 'crafted-ide-packages)
(crafted-package-install-selected-packages)
(require 'crafted-ide-config)

;;; custom-ide module
(require 'custom-ide-packages)
(crafted-package-install-selected-packages)
(require 'custom-ide-config)

;;; crafted-lisp and custom-lisp modules
(require 'crafted-lisp-packages)
(require 'custom-lisp-packages)
(crafted-package-install-selected-packages)
(require 'custom-lisp-pre-config)
(require 'crafted-lisp-config)
(require 'custom-lisp-post-config)

;;; custom-python module (resurrected from crafted-emacs/crafted-python-deprecated module)
(require 'custom-python-packages)
(crafted-package-install-selected-packages)
(require 'custom-python-config)

;;; initial-buffers module
(require 'initial-buffers-packages)
(crafted-package-install-selected-packages)
(require 'initial-buffers-config)

(provide 'init)

;; END init.el
;;

;; init.el ends here
