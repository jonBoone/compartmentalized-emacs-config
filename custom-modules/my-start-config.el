;;; my-start-config.el --- personal configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: Jon Boone

;;; Commentary:

;; bind 'my-config-file to "init.el" by default
(custom-set-default 'my-config-file "init.el")

;; eliminate having to type out "yes" and "no"
(setq use-short-answers t)

;; choose utf-8 as the default coding system
(set-default-coding-systems 'utf-8)

;; add advice to functions without warning
(setq ad-redefinition-action 'accept)

;; define macro to perform actions based on detected system-type
(defmacro my/with-system (type &rest body)
  "Evaluate BODY if 'system-type' equals TYPE."
  (declare (indent defun))
  `(when (eq system-type ',type)
     ,@body))

;; my/display-startup-time function
(defun my/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'my/display-startup-time)

;; my/minibuffer-setup-hook function
(defun my/minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(add-hook 'minibuffer-setup-hook #'my/minibuffer-setup-hook)

;; my/minibuffer-exit-hook function
(defun my/minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-exit-hook #'my/minibuffer-exit-hook)

;; my/open-config function
(defun my/open-config ()
  (interactive)
  (find-file (expand-file-name my-config-file user-emacs-directory)))

;; my/reload-config function
(defun my/reload-config ()
  "Reload init file, which will effectively reload everything"
  (interactive)
  (load-file (expand-file-name my-config-file user-emacs-directory)))


;; frame transparency setting
(defvar my/frame-transparency '(90 . 90))

;; setup buffer centering with margins
(defun my/center-buffer-with-margins ()
  (let ((margin-size (/ (- (frame-width) 132) 3)))
    (set-window-margins nil margin-size margin-size)))

(defun my/mode-visual-fill ()
  (setq visual-fill-column-width       132
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

;; capture the environment variables
(use-package exec-path-from-shell
  :straight t
  :init
  (setq exec-path-from-shell-check-startup-files nil
        exec-path-from-shell-shell-name          "/bin/zsh"
        exec-path-from-shell-variables           '("GOPATH" "LANG" "MANPATH" "PATH"))
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(provide 'my-start-config)
;; END my-start-config.el
;;

;; my-start-config.el ends here
