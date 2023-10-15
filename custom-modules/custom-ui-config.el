;;; custom-ui-config.el --- custom ui configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: Jon Boone

;;; Commentary:

;; manage popup windows with popper
(use-package popper
  :straight t
  :bind
  (("M-`"      . popper-toggle)
   ("M-~"      . popper-cycle)
   ("C-x M-`"  . popper-toggle-type))
  :config
  (setq popper-reference-buffers '(("\\*Messages\\*"             . hide)
                                   ("\\*Warnings\\*"             . hide)
                                   ("\\*xref\\*"                 . hide)
                                   ("\\*Backtrace\\*"            . hide)
                                   ("*Flymake diagnostics.*"     . hide)
                                   ("\\*eldoc\\*"                . hide)
                                   ("\\*compilation\\*"          . hide)
                                   ("\\*rustic-"                 . hide)
                                   ("^*tex"                      . hide)
                                   ("\\*Ement Notifications\\*"  . hide)
                                   ("Output\\*$"                 . hide)
                                   ("\\*Async Shell Command\\*"  . hide)
                                   ("\\*Dtache Shell Command\\*" . hide)
                                   ("\\*GDB.*out\\*"             . hide)
                                   help-mode
                                   compilation-mode)
        popper-display-control   'user)
  (popper-mode +1))

(provide 'custom-ui-config)
;; END custom-ui-config.el
;;

;; custom-ui-config.el ends here
