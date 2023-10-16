;;; custom-defaults-config.el --- custom defaults    -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: Jon Boone

;;; Commentary:

;;; Buffers

;; don't warn when reading large files
(setq large-file-warning-threshold nil)

;;; Persistence between sessions

;; configure savehist options
(setq history-length             50
      history-delete-duplicates  t)

;; remember the last cursor location of opened files
(save-place-mode 1)

(provide 'my-defaults-config)
;; END my-defaults-config.el
;;

;; my-defaults-config.el ends here

