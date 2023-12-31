;;; custom-org-packages.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: Jon Boone <ipmonger@delamancha.org>

;; Commentary

;; Install preferred version of org-mode

;;; Code:

;; Toggle the visibility of some Org elements.
(add-to-list 'package-selected-packages 'org)
(add-to-list 'package-selected-packages 'org-contrib)
(add-to-list 'package-selected-packages 'org-appear)
(add-to-list 'package-selected-packages 'org-bullets)
(add-to-list 'package-selected-packages 'org-caldav)
(add-to-list 'package-selected-packages 'org-roam)

(provide 'custom-org-packages)
;;; custom-org-packages.el ends here
