;;; custom-early-init-straight.el --- bootstrap straight.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: Jon Boone, System Crafters Community

;;; Commentary:
;;; take from System Crafters crafted-early-init-straight.el 

;;; Code:

(load (expand-file-name "compartmentalized-package-config" compartmentalized-modules))

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
(setq compartmentalized-package-system 'straight)
(setq compartmentalized-package-installer #'straight-use-package)
(setq compartmentalized-package-installed-predicate #'straight--installed-p)

(provide 'compartmentalized-early-init-straight)
;; END compartmentalized-early-init-straight.el
;;
