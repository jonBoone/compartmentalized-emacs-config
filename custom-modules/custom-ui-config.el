;;; custom-ui-config.el --- custom ui configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: Jon Boone

;;; Commentary:


;; customize truncation indicators and add breathing room at the edge of the window
(define-fringe-bitmap 'right-curly-arrow
  [#b10000000
   #b10000000
   #b01000000
   #b01000000
   #b00100000
   #b00100000
   #b00010000
   #b00010000
   #b00001000
   #b00001000
   #b00000100
   #b00000100])
(define-fringe-bitmap 'left-curly-arrow
  [#b00000100
   #b00000100
   #b00001000
   #b00001000
   #b00010000
   #b00010000
   #b00100000
   #b00100000
   #b01000000
   #b01000000
   #b10000000
   #b10000000])

(set-fringe-mode 10)

;; use visual bell
(setq visible-bell t)

;; narrow the gap betwen emacs frames and other os windows
(setq frame-resize-pixelwise t)

;; don't pop up UI dialogs when prompting
(setq use-dialog-box nil)

;; turn on column numbers in addition to line numbers
(column-number-mode)

;; enable line numbers by default
(global-display-line-numbers-mode t)

;; don't display line numbers for these modes
(dolist (mode '(comint-mode-hook treemacs-mode-hook))
  (add-hook mode 'crafted-ui--disable-line-numbers-mode))

;; set the frame transparency
(add-to-list 'default-frame-alist `(alpha . ,my/frame-transparency))

;; ensure that block cursor when using xlib is the same size as the glyph
(setq x-stretch-cursor t)

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
