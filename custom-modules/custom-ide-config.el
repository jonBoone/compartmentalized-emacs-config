;;; custom-ide-config.el --- custom ide configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: Jon Boone

;;; Commentary:

;; ensure we have more space when editing a parent-sexp
(use-package expand-region
  :straight t
  :bind
  (("M-[" . er/expand-region)
   ("C-(" . er/mark-outside-pairs)))

(provide 'custom-ide-config)
;; END custom-ide-config.el
;;

;; custom-ide-config.el ends here
