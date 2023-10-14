;;; keybinding-libraries-config.el --- keybinding-libraries module  -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: Jon Boone

;;; Commentary:

;; `ESC` Cancels Out Anywhere
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Keybinding Panel `(which-key)`
(use-package which-key
  :straight t
  :defer t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))


(provide 'keybinding-libraries-config)
;; END keybinding-libraries-config.el
;;

;; keybinding-libraries-config.el ends here
