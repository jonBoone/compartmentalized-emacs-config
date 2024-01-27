;;;; compartmentalized-init-config.el --- Compartmentalized Emacs initial configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community, Jon Boone

;;; Commentary:

;; Perform some initialization for use by Compartmentalized Emacs modules among
;; other things.

;;; Code:

;; Use this file to find the project root where you cloned
;; `compartmentalized-emacs' and use that as the value for the
;; `compartmentalized-emacs-home' value which is needed by a few modules,
;; including the template below used for writing Compartmentalized Emacs
;; modules.

(defgroup compartmentalized-init '()
  "Initialization configuration for Compartmentalized Emacs"
  :tag "Compartmentalized Init"
  :group 'compartmentalized)

;; By default, compartmentalized emacs calls `customize-save-variable' in the
;; `after-init-hook'. The user can opt out of this by setting
;; `compartmentalized-init-auto-save-customized' to nil.
(defcustom compartmentalized-init-auto-save-customized t
  "Save customized variables automatically every session."
  :type 'boolean
  :group 'compartmentalized-init)

;; By default, compartmentalized emacs calls `package--save-selected-packages' in the
;; `after-init-hook'. The user can opt out of this by setting
;; `compartmentalized-init-auto-save-selected-packages' to nil.
(defcustom compartmentalized-init-auto-save-selected-packages t
  "Save the list of selected packages automatically every session."
  :type 'boolean
  :group 'compartmentalized-init)

(when (version< emacs-version "29")
  ;; Get some Emacs 29 compatibility functions. Notably missing is
  ;; `setopt' which the `compat' library deliberately does not
  ;; provide, so we continue to use the `customize-set-variable'
  ;; function for setting user options, unless we have a version guard
  ;; around a block, in which case we use `setopt' instead.
  (unless (require 'compat nil :noerror)
    (package-install 'compat)))

(require 'project)

;; If the source file is newer than the compiled file, load it instead
;; of the compiled version.
(customize-set-variable 'load-prefer-newer t)

;; Create the variable if needed
(if (boundp 'compartmentalized-emacs-home)
    (message "compartmentalized-emacs-home value set by user: %s" compartmentalized-emacs-home)
  (defvar compartmentalized-emacs-home nil
    "Defines where the Compartmentalized Emacs project was cloned to.

This is set when loading the compartmentalized-init-config.el module during
initialization.  Alternatively, it can be set by the user
explicitly."))

;; Only set the `compartmentalized-emacs-home' variable if it does not already
;; have a value set by the user.
(when (null compartmentalized-emacs-home)
  (setq compartmentalized-emacs-home
        (expand-file-name
         (vc-find-root load-file-name "modules"))))

;; we still don't have a `compartmentalized-emacs-home' value, so we can't
;; proceed, without it the `load-path' will not be set correctly and
;; compartmentalized-emacs modules will not be found.
(unless compartmentalized-emacs-home
  (error "%s\n%s"
         "The value for compartmentalized-emacs-home is not set"
         "Please set this value to the location where compartmentalized-emacs is installed"))

;; update the `load-path' to include the Compartmentalized Emacs modules path

(let ((modules (expand-file-name "./modules/" compartmentalized-emacs-home)))
  (when (file-directory-p modules)
    (message "adding modules to load-path: %s" modules)
    (add-to-list 'load-path modules)))

;; If a `custom-modules' directory exists in the
;; `user-emacs-directory', include it on the load-path.
(let ((custom-modules (expand-file-name "custom-modules" user-emacs-directory)))
  (when (file-directory-p custom-modules)
    (message "adding custom-modules to load-path: %s" custom-modules)
    (add-to-list 'load-path custom-modules)))

;; When writing compartmentalized-modules, insert header from skeleton
(auto-insert-mode)
(with-eval-after-load "autoinsert"
  ;; Handle a missing `custom-file' by not running `auto-insert' when
  ;; it gets created.  The value of the `custom-file' for Compartmentalized
  ;; Emacs is `custom.el', however, the user could change that to
  ;; something else.  On startup, asking the user to automatically
  ;; insert the standard headers may cause confusion if they choose to
  ;; answer 'y'.  Here we advise the `auto-insert' function to not run
  ;; when the file is the `custom-file' and it is being created.
  (defun ignore-auto-insert-for-custom (orig-auto-insert &rest args)
    "Apply ORIG-AUTO-INSERT only when the file is not the
         `custom-file' to avoid confusion when that file doesn't exist on
         startup."
    (if (and custom-file buffer-file-name
             (string-match (file-name-nondirectory custom-file) buffer-file-name))
        (message "Skipping auto-insert for %s" custom-file)
      (apply orig-auto-insert args)))
  (advice-add 'auto-insert :around #'ignore-auto-insert-for-custom)
  (define-auto-insert
    (cons (expand-file-name "modules/compartmentalized-.*\\.el" compartmentalized-emacs-home)
          "Compartmentalized Emacs Lisp Skeleton")
    '("Compartmentalized Emacs Module Description: "
      ";;;; " (file-name-nondirectory (buffer-file-name)) " --- " str
         (make-string (max 2 (- 80 (current-column) 27)) ?\s)
         "-*- lexical-binding: t; -*-" '(setq lexical-binding t)
         "

;; Copyright (C) " (format-time-string "%Y") "
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary:

;; " _ "

;;; Code:

(provide '"
         (file-name-base (buffer-file-name))
         ")
;;; " (file-name-nondirectory (buffer-file-name)) " ends here\n")))

;; Add the Compartmentalized Emacs documentation to the info nodes
(let ((compartmentalized-info-dir (expand-file-name "docs/dir" compartmentalized-emacs-home)))
  (when (file-exists-p compartmentalized-info-dir)
    (require 'info)
    (info-initialize)
    (push (file-name-directory compartmentalized-info-dir) Info-directory-list)))

(defun compartmentalized-save-customized ()
  "Save and reload the customizations made during Emacs initialization.

Due to the way Emacs Customization works - or seems to - and this
bug: https://debbugs.gnu.org/cgi/bugreport.cgi?bug=21355, we need
to save all customizations made during Emacs startup and then
reload the custom-file.  This sets (or should set) all customized
values to the \"SET and saved.\" state and (hopefully) avoid the
bug above.  If the user never set a value for `custom-file' then
we can't reload the file."
  (customize-save-customized)
  ;; only load the `custom-file' if it is not `nil'. 
  (when custom-file
    (load custom-file :noerror)))

;; Save all customizations to `custom-file', unless the user opted out.
(when compartmentalized-init-auto-save-customized
  (add-hook 'after-init-hook #'compartmentalized-save-customized))
(when compartmentalized-init-auto-save-selected-packages
  (add-hook 'after-init-hook #'package--save-selected-packages))

(provide 'compartmentalized-init-config)
;;; compartmentalized-init-config.el ends here
