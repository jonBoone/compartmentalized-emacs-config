;;; initial-buffers-config.el --- initial buffers config  -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: Jon Boone

;;; Commentary:

;; don't show the startup message
(setq inhibit-startup-screen            t
      inhibit-startup-echo-area-message t
      inhibit-startup-message           t
      inhibit-scratch-message           t)

;; show a dashboard upon start
(use-package dashboard
  :straight t
  :init
  (setq dashboard-banner-logo-title "Iain's Dashboard"
        dashboard-center-content    t
        dashboard-display-icons-p   t
        dashboard-items             '((agenda . 16) (projects . 5))
        dashboard-icon-type         'nerd-icons
        dashboard-projects-backend  'project-el
        dashboard-set-file-icons    t
        dashboard-set-footer        nil
        dashboard-set-heading-icons t
        dashboard-set-init-info     t
        dashboard-set-navigator     t
        dashboard-startup-banner    (concat user-emacs-directory "Emacs-Is-Sexy-V2-64x64.png")
        dashboard-week-agenda       t)
  :config
  (dashboard-setup-startup-hook))

(provide 'initial-buffers-config)
;; END initial-buffers-config.el
;;

;; initial-buffers-config.el ends here
