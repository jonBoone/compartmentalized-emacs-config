;;; additional-elisp-libraries-packages.el --- add-on elisp libraries module  -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: Jon Boone

;;; Commentary:

;;; Code:

;; comment tags highlighting and listing capabilities
(add-to-list 'package-selected-packages 'comment-tags)

;; modern list libary
(add-to-list 'package-selected-packages 'dash)

;; hash table library
(add-to-list 'package-selected-packages 'ht)

;; string-manipulation library
(add-to-list 'package-selected-packages 's)

;; timestamp and date library
(add-to-list 'package-selected-packages 'ts)

(provide 'additional-elisp-libraries-packages)
;; END additional-elisp-libraries-packages.el
;;

;; additional-elisp-libraries-packages.el ends here
