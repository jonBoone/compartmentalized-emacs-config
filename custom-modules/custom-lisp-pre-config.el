;;; custom-lisp-pre-config.el --- custom lisp pre-config -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: Jon Boone

;;; Commentary:

;; use lispy package
(use-package lispy
  :straight t
  :hook (emacs-lisp-mode lisp-mode scheme-mode))

;; use parinfer for lisp languages
(use-package parinfer
  :hook
  (clojure-mode emacs-lisp-mode common-lisp-mode scheme-mode lisp-mode)
  :config
  (setq parinfer-extensions
        '(defaults      ; should be included
          pretty-parens ; different paren styles for different modes
          smart-tab     ; C-b & C-f jump positions and smart shift with tab & S-tab
          smart-yank))) ; Yank behavior depends on mode

;; use smartparens
(use-package smartparens
  :straight t
  :hook prog-mode
  :config
  (sp-with-modes '(org-mode)
    (sp-local-pair "*" "*")))

;; set these values prior to loading sly package
;; set these values prior to loading sly
(defvar lisp-images-base-dir
  (concat (get 'abbreviated-home-dir 'home) "/.local")
  "This is the base directory in which to find various cl implementations.")

(defvar sbcl-core-for-sly
  "/lib/sbcl/sbcl.core-for-sly"
  "This is the relative path to the sly-specific core for sbcl.")

(defvar sbcl-latest-sly-core-file
  (concat lisp-images-base-dir "/sbcl/latest" sbcl-core-for-sly)
  "This is where to find the sly-customized core file.")

(setq sly-contribs             '(sly-fancy sly-retro sly-trace-dialog)
      sly-lisp-implementations `((sbcl-latest ("sbcl" "--core"
                                               ,Sbcl-latest-sly-core-file))
                                 (cmucl-latest ("lisp")))
      sly-net-coding-system 'utf-8-unix)

(provide 'custom-lisp-pre-config)
;; END custom-lisp-pre-config.el
;;

;; custom-lisp-pre-config.el ends here
