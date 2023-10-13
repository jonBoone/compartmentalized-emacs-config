;;; custom-straight-config.el --- customized straight configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: Jon Boone

;;; Commentary:
;; I want configure use-package to leverage straight
(setq use-package-always-ensure nil    ; avoid forcing the use of package.el
      use-package-verbose       'debug
      )

;; leverage 'use-package to setup straight!
(use-package straight
  :custom
  (straight-host-usernames          '((github . "jonBoone")))
  (straight-vc-git-default-protocol 'ssh)
  (straight-use-package-by-default  t)
  ;; ensure we pull the latest org-mode related packages by pre-registering them
  (straight-register-package 'org)
  (straight-register-package 'org-contrib))

