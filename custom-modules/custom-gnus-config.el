;;; custom-gnus-config.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: Jon Boone <ipmonger@delamancha.org>

;; Commentary

;; Provides basic configuration for Gnus

;;; Code:

(use-package epa
  :defer t
  :config
  (setq epa-popup-info-window nil
        epa-pinentry-mode     'loopback)
  (epa-file-enable))

(use-package epg
  :defer t
  :config
  (custom-set-variables '(epg-gpg-program  "/opt/homebrew/bin/gpg")))

;;; Gnus

;; define gnus-user-format-function-date
(defun gnus-user-format-function-d (header)
  ;; Print out date in yyyy-mm-dd hh:mm:ss
  (format-time-string "%Y-%m-%d %T" (gnus-date-get-time (mail-header-date header))))
(use-package gnus
  :defer t
  :config
  (gnus-delay-initialize))


(provide 'custom-gnus-config)
;;; custom-gnus-config.el ends here
