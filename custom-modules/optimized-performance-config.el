;;; optimized-performance-config.el --- performance optimization module  -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: Jon Boone

;;; Commentary:

;;; Code:

;; async behavior
(use-package async
  :straight t
  :defer t
  :init (dired-async-mode 1))

(provide 'optimized-performance-config)
;; END optimized-performance-config.el
;;

;; optimized-performance-config.el ends here
