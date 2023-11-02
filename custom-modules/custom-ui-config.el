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


;;;; My custom visual settings

;; color theme
(use-package doom-themes
  :straight t
  :init
  (load-theme 'doom-nord t)
  (doom-themes-visual-bell-config))

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
(dolist (mode '(shell-mode-hook term-mode-hook vterm-mode-hook comint-mode-hook treemacs-mode-hook))
  (add-hook mode 'crafted-ui--disable-line-numbers-mode))

;; set the frame transparency
(add-to-list 'default-frame-alist `(alpha . ,my/frame-transparency))

;; ensure that block cursor when using xlib is the same size as the glyph
(setq x-stretch-cursor t)

;;;; Mode Line

(setq display-time-format "%l:%M: %p %b %y" display-time-default-load-average nil)

(use-package smart-mode-line
  :straight t
  :init
  (setq sml/no-confirm-load-theme t
        sml/mode-width            'right
        sml/name-width            60)
  :config
  (sml/setup)
  (sml/apply-theme 'respectful)
  (setq-default mode-line-format '("%e"
                                   mode-line-front-space
                                   mode-line-client
                                   mode-line-modified
                                   mode-line-remote
                                   mode-line-frame-identification
                                   mode-line-buffer-identification
                                   sml/pos-id-separator
                                   (vc-mode vc-mode)
                                   " "
                                   sml/pre-modes-separator
                                   mode-line-modes
                                   " "
                                   mode-line-misc-info)))

(use-package minions
  :straight t
  :after doom-modeline-mode
  :hook doom-modeline-mode)

(use-package nerd-icons-ibuffer
  :straight t
  :config
  (nerd-icons-completion-mode))

(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode)
  :custom-face
  (mode-line           ((t (:height 0.90))))
  (mode-line-inactive  ((t (:height 0.85))))
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height                  15
        doom-modeline-bar-width               6
        doom-modeline-buffer-file-name-style  'relative
        doom-modeline-buffer-state-icon       t
        doom-modeline-github                  t
        doom-modeline-hud                     t
        doom-modeline-icon                    t
        doom-modeline-eglot                   t
        doom-modeline-major-mode-color-icon   t
        doom-modeline-major-mode-icon         t
        doom-modeline-minor-mode-icon         t
        doom-modeline-minmor-modes            t
        doom-modeline-project-detection       t
        doom-modeline-unicode-fallback        t))

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

(use-package winner
  :straight nil
  :bind
  (("s-/" . winner-undo)
   ("s-?" . winner-redo))
  :config
  (winner-mode))

(provide 'custom-ui-config)
;; END custom-ui-config.el
;;

;; custom-ui-config.el ends here
