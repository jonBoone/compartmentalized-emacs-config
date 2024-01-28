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

;;; compartmentalized-init-config
(require 'compartmentalized-init-config)

;;; leverage my-start module
(require 'my-start-config)

;;; leverage my-font module
(require 'my-font-config)

;; additional elisp libraries
(require 'additional-elisp-libraries-packages)
(compartmentalized-package-install-selected-packages)
(require 'additional-elisp-libraries-config)

;;; leverage performance-optimization module
(require 'optimized-performance-packages)
(compartmentalized-package-install-selected-packages)
(require 'optimized-performance-config)


;; workspace configurations
(require 'custom-workspaces-packages)
(compartmentalized-package-install-selected-packages)
(require 'custom-workspaces-config)

;; keybinding libraries module
(require 'keybinding-libraries-packages)
(compartmentalized-package-install-selected-packages)
(require 'keybinding-libraries-config)

;;; leverage custom-defaults module
(require 'custom-defaults-packages)
(compartmentalized-package-install-selected-packages)
(require 'custom-defaults-config)

;;; add support for org-mode
(require 'custom-org-packages)
(compartmentalized-package-install-selected-packages)
(require 'custom-org-config)

;;; crafted-defaults - the General Crafted Emacs endorsed defaults
(require 'compartmentalized-defaults-config)

;;; leverage my-files module
(require 'my-files-packages)
(compartmentalized-package-install-selected-packages)
(require 'my-files-config)

;;; custom-ui-mode
(require 'custom-ui-packages)
(compartmentalized-package-install-selected-packages)
(require 'custom-ui-config)

;;; crafted-completion module
(require 'compartmentalized-completion-packages)
(compartmentalized-package-install-selected-packages)
(require 'compartmentalized-completion-config)

;;; custom-browser module
(require 'custom-browser-config)

;;; custom-term module
(require 'custom-term-packages)
(compartmentalized-package-install-selected-packages)
(require 'custom-term-config)

;;; custom-gnus module
(require 'custom-gnus-packages)
(compartmentalized-package-install-selected-packages)
(require 'custom-gnus-config)

;;; compartmentalized-writing module
(require 'compartmentalized-writing-packages)
(compartmentalized-package-install-selected-packages)
(require 'compartmentalized-writing-config)

;;; custom-writing module
(require 'custom-writing-packages)
(compartmentalized-package-install-selected-packages)
(require 'custom-writing-config)

;;; compartmentalized-ide module
(require 'compartmentalized-ide-packages)
(compartmentalized-package-install-selected-packages)
(require 'compartmentalized-ide-config)

;;; custom-ide module
(require 'custom-ide-packages)
(compartmentalized-package-install-selected-packages)
(require 'custom-ide-config)

;;; crafted-lisp and custom-lisp modules
(require 'crafted-lisp-packages)
(require 'custom-lisp-packages)
(compartmentalized-package-install-selected-packages)
(require 'custom-lisp-pre-config)
(require 'crafted-lisp-config)
(require 'custom-lisp-post-config)

;;; custom-python module (resurrected from crafted-emacs/crafted-python-deprecated module)
(require 'custom-python-packages)
(compartmentalized-package-install-selected-packages)
(require 'custom-python-config)

;;; initial-buffers module
(require 'initial-buffers-packages)
(compartmentalized-package-install-selected-packages)
(require 'initial-buffers-config)

(provide 'init)

;; END init.el
;;

;; init.el ends here
