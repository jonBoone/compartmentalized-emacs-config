;;; my-start-config.el --- personal configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: Jon Boone

;;; Commentary:

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

;; setup a process for counting words in a buffer
(defun my/count-words-buffer ()
  "Count the number of words in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((count 0))
      (while (not (eobp))
        (forward-word 1)
        (setq count (1+ count)))
      count)))
  
(defvar count-words-buffer nil
  "*Number of words in the buffer.")

(defun my/update-wc ()
  (interactive)
  (setq count-words-buffer (number-to-string (my/count-words-buffer)))
  (force-mode-line-update))

(unless count-words-buffer
  ;; seed count-words-paragraph
  ;; create timer to keep count-words-paragraph updated
  (run-with-idle-timer 1 t 'my/update-wc))

;; add count words paragraph to the mode line-mode
(unless (memq 'count-words-buffer global-mode-string)
  (add-to-list 'global-mode-string "words: " t)
  (add-to-list 'global-mode-string 'my/count-words-buffer t))


(provide 'my-start-config)
;; END my-start-config.el
;;

;; my-start-config.el ends here
