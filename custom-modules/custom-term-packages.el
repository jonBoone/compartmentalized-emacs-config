;;; custom-term-packages.el --- term packages -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier:: MIT

;; Author: Jon Boone <ipmonger@delamancha.org>

;;; Commentary:

;;; Code
(add-to-list 'package-selected-packages 'eterm-256color)
(add-to-list 'package-selected-packages 'term)
(add-to-list 'package-selected-packages 'vterm)
(add-to-list 'package-selected-packages 'multi-vterm)

(provide 'custom-term-packages)
;; END custom-term-packages.el
;;

;; custom-term-packages.el ends here
