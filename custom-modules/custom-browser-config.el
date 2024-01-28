;;; custom-browser-config.el --- custom defaults    -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: Jon Boone

;;; Commentary:

(use-package ctable
  :straight t)


(use-package browse-url
  :straight t
  :init
  ;; This function always opens a new website in a new window
  (defun xwidget-browse-url-no-reuse (url &optional session)
    (interactive (progn
                   (browse-url-interactive-arg "xwidget-webkit URL: ")))
    (xwidget-webkit-browse-url url t))
  (defun browse-url-adjust-size-dispatch ()
    (when (equal major-mode 'xwidget-webkit-mode)
      (xwidget-webkit-adjust-size-dispatch)))
  (customize-set-value browse-url-browser-function
                       (lambda (url session)
                         (other-window 1)
                         (xwidget-browse-url-no-reuse url))))


(use-package xwwp-full
  :straight (xwwp-full :host github :repo "kchanqvq/xwwp")
  :hook
  ;; adapt webkit according to window configuration change automatically
  ;; without this hook, every time you change your window configuration,
  ;; you must press 'a' to adapt webkit content to new window size
  (window-configuration-change-hook . browse-url-adjust-size-dispatch)
  :bind
  (:map xwidget-webkit-mode-map
        ("<localleader>b"      . 'xwidget-webkit-back)
        ("<localleader>j"      . 'xwwp-ace-toggle)
        ("<localleader>o"      . 'xwwp-section)
        ("<localleader>h"      . 'xwwp-history-show)
        ("<localleader><up>"   . 'xwidget-webkit-scroll-down)
        ("<localleader><down>" . 'xwidget-webkit-scroll-up)
        ;; ((kbd "M-w")           . 'xwidget-webkit-copy-selection-as-kill)
        )
  :init
  :config
  (require 'cl-lib)
  (customize-set-value xwwp-follow-link-completion-system 'default))


(provide 'custom-browser-config)
;; END custom-browser-config.el
;;

;; custom-browser-config.el ends here
