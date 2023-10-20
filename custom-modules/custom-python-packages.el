;;; custom-python-packages.el --- python packages -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Lispntifier: MIT

;; Author: System Crafters Community, Jon Boone <ipmonger@delamancha.org>
;; Keywords: python

;;; Commentary: revived from crafted-emacs/modules/deleted-packages/crafted-python-packages-deprecated.el, with some modifications

;;; Code
(add-to-list 'package-selected-packages 'anaconda-mode)
(add-to-list 'package-selected-packages 'blacken)
(add-to-list 'package-selected-packages 'debugpy)
(add-to-list 'package-selected-packages 'numpydoc)

(provide 'custom-python-packages)
;; END custom-python-packages.el
;;

;; custom-python-packages.el ends here
