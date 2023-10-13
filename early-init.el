;;; early-init.el --- Bootstrap straight.el via crafted-eamcs V2 RC1 -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-Licence-Identifier: MIT

;; Author: Jon Boone

;;; Commentary:

;; Code to bootstrap straight package manager
;;
;; For use in early-init.el!

;; minimize gc runs during loading (and possibly updating) config
(setq gc-cons-threshold (* 200 1024 1024))

;; disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

;; don't process file-handlers yet
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

;; don't search for init.el in site-lisp
(setq site-run-file nil)

;; remove unnecessary UI elements
(unless (and (display-graphic-p) (eq system-type 'darwin))
  (push '(menu-bar-lines . 0) default-frame-alist))
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . 0) default-frame-alist)

;; native compiler support
(message (concat
          "Native compilation is "
          (if (and (fboundp 'native-comp-available-p) (native-comp-available-p))
              (progn
                (setq comp-deferred-compilation t)
                "")
            "*not* ")
          "available"))


;;; cl-lib - for 'cl-remove
(require 'cl-lib)

;; ensure we don't use the included version of org-mode
(setq load-path (cl-remove "org$" load-path :test 'string-match-p))

;; load the straight support module and dependencies
(setq crafted-emacs-home "~/.config/emacs/crafted-emacs/")
(add-to-list 'load-path crafted-emacs-home)
(setq crafted-emacs-modules (expand-file-name "modules/" crafted-emacs-home))
(add-to-list 'load-path crafted-emacs-modules)
(setq custom-modules (expand-file-name "custom-modules/" user-emacs-directory))
(add-to-list 'load-path custom-modules)

(require 'crafted-early-init-straight)
(require 'crafted-package-config)
(require 'custom-straight-packages)
(crafted-package-install-selected-packages)
(require 'custom-straight-config)

(provide 'early-init)

;; END early-init.el
;;

;; early-init.el ends here
