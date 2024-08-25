;;; my-yow-config.el --- zippy quotes module config  -*- lexical-binding: t; -*-

;; Copyright (C) 2024
;; SPDX-License-Identifier: MIT

;; Author: Jon Boone

;;; Commentary:
;;; leverages:
;;;   yow.el from https://www.emacswiki.org
;;;   yow.lines from https://github.com/vim-scripts/Yow

;; configure location of yow.lines
(customize-set-variable
 'yow-file
 (expand-file-name "yow.lines"
                   (concat (get 'abbreviated-home-dir
                                'home)
                           "/.local/share/etc/")))

(add-to-list 'package-selected-packages 'yow)

(provide 'my-yow-config)
;; END my-yow-config
;;

;; my-yow-config.el ends here
