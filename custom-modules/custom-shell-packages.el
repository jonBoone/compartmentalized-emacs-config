;;; custom-shell-packages.el --- shell packages -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: Jon Boone <ipmonger@delamancha.org>

;;; Commentary:

;;; Code
(add-to-list 'package-selected-packages 'eshell)
(add-to-list 'package-selected-packages 'eshell-git-prompt)

(provide 'custom-shell-packages)
;; END custom-shell-packages.el
;;

;; custom-shell-packages.el ends here
