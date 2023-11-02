;;; custom-gnus-packages.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: Jon Boone <ipmonger@delamancha.org>

;; Commentary

;; GNUs and supporting packages

;;; Code:

(add-to-list 'package-selected-packages 'epa)
(add-to-list 'package-selected-packages 'epg)
(add-to-list 'package-selected-packages 'gnus)
(add-to-list 'package-selected-packages 'message)

(provide 'custom-gnus-packages)
;;; custom-gnus-packages.el ends here
