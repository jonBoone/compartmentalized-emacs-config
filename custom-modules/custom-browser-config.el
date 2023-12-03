;;; custom-browser-config.el --- custom defaults    -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: Jon Boone

;;; Commentary:


(use-package xwwp-full
  :straight (xwwp-full :host github :repo "kchanqvq/xwwp")
  :bind
  (:map xwidget-webkit-mode-map
        ("<localleader>b" . 'xwidget-webkit-back)
        ("<localleader>j" . 'xwwp-ace-toggle)
        ("<localleader>o" . 'xwwp-section)
        ("<localleader>h" . 'xwwp-history-show))
  :init
  (customize-set-value xwwp-follow-link-completion-system 'default)
  :config
  (require 'cl-lib))


(provide 'custom-browser-config)
;; END custom-browser-config.el
;;

;; custom-browser-config.el ends here
