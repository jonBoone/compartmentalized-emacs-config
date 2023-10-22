;;; custom-straight-config.el --- customized straight configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: Jon Boone

;;; Commentary:

;; I want configure use-package to leverage straight
(setq use-package-always-ensure nil    ; avoid forcing the use of package.el
      use-package-verbose       'debug)

;; add package sources if they are not already present
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
(unless (assoc-default "nongnu" package-archives)
  (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t))

(customize-set-variable 'package-archive-priorities
      '(("gnu"      . 90)      ; highest spot
        ("nongnu"   . 80)      ; second spot
        ("melpa"    . 70)      ;
        ))

;; leverage 'use-package to setup straight!
(use-package straight
  :custom
  (straight-host-usernames          '((github . "jonBoone")))
  (straight-vc-git-default-protocol 'ssh)
  (straight-use-package-by-default  t)
  ;; ensure we pull the latest org-mode related packages by pre-registering them
  (straight-register-package 'org)
  (straight-register-package 'org-contrib))

;; change the association of "python-mode" with python-mode.el rather than python.el
(autoload 'python-mode "python-mode" "Python Mode." t)

(provide 'custom-straight-config)
;; END custom-straight-config.el
;;

;; custom-straight-config.el ends here
