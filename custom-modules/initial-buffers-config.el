;;; initial-buffers-config.el --- initial buffers config  -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: Jon Boone

;;; Commentary:

;; don't show the startup message
(setq inhibit-startup-message t)

;; show a dashboard upon start
(use-package dashboard
  :straight t
  :config
  (setq dashboard-banner-logo-title "Iain's Dashboard"
        dashboard-startup-banner    (concat user-emacs-directory "Emacs-Is-Sexy-V2-64x64.png")
        dashboard-center-content    t
        dashboard-items             '((agenda . 16))
        dashboard-icon-type         'nerd-icons
        dashboard-display-icons-p   t)
        (dashboard-setup-startup-hook))

(provide 'initial-buffers-config)
;; END initial-buffers-config.el
;;

;; initial-buffers-config.el ends here
