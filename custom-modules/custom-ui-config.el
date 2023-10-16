;;; custom-ui-config.el --- custom ui configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: Jon Boone

;;; Commentary:

;; Some of these settings are from the System Crafters Community
;; crafted-emacs v2 crafted-ui-config.el file

;;;; Help Buffers

;; Make `describe-*' screens more helpful
(when (require 'helpful nil :noerror)
  (keymap-set helpful-mode-map "<remap> <revert-buffer>" #'helpful-update)
  (keymap-global-set "<remap> <describe-command>"        #'helpful-command)
  (keymap-global-set "<remap> <describe-function>"       #'helpful-callable)
  (keymap-global-set "<remap> <describe-key>"            #'helpful-key)
  (keymap-global-set "<remap> <describe-symbol>"         #'helpful-symbol)
  (keymap-global-set "<remap> <describe-variable>"       #'helpful-variable)
  (keymap-global-set "C-h F"                             #'helpful-function))

;; Bind extra `describe-*' commands
(keymap-global-set "C-h K" #'describe-keymap)

;;;; Line Numbers
(defcustom crafted-ui-line-numbers-enabled-modes
  '(conf-mode prog-mode)
  "Modes which should display line numbers."
  :type 'list
  :group 'crafted-ui)

(defcustom crafted-ui-line-numbers-disabled-modes
  '(org-mode)
  "Modes which should not display line numbers.

Modes derived from the modes defined in
`crafted-ui-line-number-enabled-modes', but should not display line numbers."
  :type 'list
  :group 'crafted-ui)

(defun crafted-ui--enable-line-numbers-mode ()
  "Turn on line numbers mode.

Used as hook for modes which should display line numbers."
  (display-line-numbers-mode 1))

(defun crafted-ui--disable-line-numbers-mode ()
  "Turn off line numbers mode.

Used as hook for modes which should not display line numebrs."
  (display-line-numbers-mode -1))

(defun crafted-ui--update-line-numbers-display ()
  "Update configuration for line numbers display."
  (if crafted-ui-display-line-numbers
      (progn
        (dolist (mode crafted-ui-line-numbers-enabled-modes)
          (add-hook (intern (format "%s-hook" mode))
                    #'crafted-ui--enable-line-numbers-mode))
        (dolist (mode crafted-ui-line-numbers-disabled-modes)
          (add-hook (intern (format "%s-hook" mode))
                    #'crafted-ui--disable-line-numbers-mode))
        (setq-default
         display-line-numbers-grow-only t
         display-line-numbers-type t
         display-line-numbers-width 2))
     (progn
       (dolist (mode crafted-ui-line-numbers-enabled-modes)
         (remove-hook (intern (format "%s-hook" mode))
                      #'crafted-ui--enable-line-numbers-mode))
       (dolist (mode crafted-ui-line-numbers-disabled-modes)
         (remove-hook (intern (format "%s-hook" mode))
                      #'crafted-ui--disable-line-numbers-mode)))))

(defcustom crafted-ui-display-line-numbers nil
  "Whether line numbers should be enabled."
  :type 'boolean
  :group 'crafted-ui
  :set (lambda (sym val)
         (set-default sym val)
         (crafted-ui--update-line-numbers-display)))

;;;; Elisp-Demos

;; also add some examples
(require 'elisp-demos)
(advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)

;; add visual pulse when changing focus, like beacon but built-in
;; from from https://karthinks.com/software/batteries-included-with-emacs/
(defun pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-one-line (point)))

(dolist (command '(scroll-up-command
                   scroll-down-command
                   recenter-top-bottom
                   other-window))
  (advice-add command :after #'pulse-line))


;;;; My custom settings

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
