;;; my-start-config.el --- personal configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: Jon Boone

;;; Commentary:

;; eliminate having to type out "yes" and "no"
(fset 'yes-or-no-p 'y-or-n-p)

;; choose utf-8 as the default coding system
(set-default-coding-systems 'utf-8)

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
  (find-file config-file))

;; my/reload-config function
(defun my/reload-config ()
  "Reload init file, which will effectively reload everything"
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory)))

;; define macro to perform actions based on detected system-type
(defmacro my/with-system (type &rest body)
  "Evaluate BODY if 'system-type' equals TYPE."
  (declare (indent defun))
  `(when (eq system-type ',type)
     ,@body))

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

(provide 'my-start-config)
;; END my-start-config.el
;;

;; my-start-config.el ends here
