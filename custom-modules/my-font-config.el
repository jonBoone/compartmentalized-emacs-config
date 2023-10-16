;;; my-font-config.el --- personal configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: Jon Boone

;;; Commentary:

;; setup default font sizes
(defvar my/default-font-size nil)
(defvar my/default-variable-font-size nil)

(my/with-system darwin
  (setq my/default-font-size          160
        my/default-variable-font-size 160))
(my/with-system gnu/linux
  (setq my/default-font-size          240
        my/default-variable-font-size 240))

;; setup preferred fonts
(defvar my/fixed-pitch-font nil)
(defvar my/variable-pitch-font nil)

(my/with-system darwin
  (set-face-attribute 'default nil
                      :font "MesloLGS Nerd Font" :height my/default-font-size)
  (set-face-attribute 'fixed-pitch nil
                      :font "MesloLGS Nerd Font Mono" :height my/default-font-size)
  (set-face-attribute 'variable-pitch nil
                      :font "Cantarell" :height my/default-font-size :weight 'regular))

;; use the nerd-icons
(use-package nerd-icons
  :straight t)


(provide 'my-font-config)
;; END my-font-config.el
;;

;; my-font-config.el ends here
