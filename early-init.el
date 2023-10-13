;;; early-init.el --- Bootstrap straight.el via crafted-eamcs V2 RC1 -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-Licence-Identifier: MIT

;; Author: Jon Boone

;;; Commentary:

;; Code to bootstrap straight package manager
;;
;; For use in early-init.el!

;; minimize gc runs
(setq gc-cons-threshold (* 200 1024 1024))

;; remove unnecessary UI elements
(unless (and (display-graphic-p) (eq system-type 'darwin))
  (push '(menu-bar-lines . 0) default-frame-alist))
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . 0) default-frame-alist)

(setq crafted-emacs-home "~/.config/emacs/crafted-emacs")
(load (expand-file-name "custom-modules/crafted-early-init-straight"
			user-emacs-directory))
