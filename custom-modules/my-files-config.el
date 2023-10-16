;;; my-files-config.el --- file handling module config  -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: Jon Boone

;;; Commentary:



;; leverage gpg for encryption
(with-system darwin
             (setq epg-gpg-program "gpg"))

(with-system gnu/linux
             (setq epg-gpg-program "gpg2"))

(setq auth-sources-debug   t
      auth-sources         '((:source "~/.authinfo.gpg"))
      epa-file-encrypt-to  '("ipmonger@delamancha.org"))


;; leverage super-save package
(use-package super-save
  :defer t
  :diminish super-save-mode
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t))

(provide 'my-files-config)
;; END my-files-config.el
;;

;; my-files-config.el ends here
