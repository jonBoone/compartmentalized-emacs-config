;;; custom-early-init-straight.el --- bootstrap straight.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: Jon Boone, System Crafters Community

;;; Commentary:
;;; take from System Crafters crafted-early-init-straight.el 

;;; Code:

(load (expand-file-name "custom-modules/custom-package-config"))

;; See https://github.com/radian-software/straight.el#getting-started
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Configure crafted-emacs to use straight as package manager.
;; See `(info "(crafted-emacs)Using alternate package managers")'
(setq custom-package-system 'straight)
(setq custom-package-installer #'straight-use-package)
(setq custom-package-installed-predicate #'straight--installed-p)

(provide 'custom-early-init-straight)
;; END custom-early-init-straight.el
;;
