;;; custom-python-config.el --- python configuration      -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community, Jon Boone <ipmonger@delamancha.org>
;; Keywords: python

;;; Commentary: Revived from crafted-emacs/modules/deleted-packages/crafted-python-config-deprecated.el, with some modifications from me

;; Python development environment configuration.  Several python
;; packages can be installed with `pip'. Many of these are needed by
;; the Emacs packages used in this configuration.

;; * autopep8      -- automatically formats python code to conform to PEP 8 style guide
;; * black         -- uncompromising code formatter
;; * flake8        -- style guide enforcement
;; * importmagic   -- automatically add, remove, manage imports
;; * ipython       -- interactive python shell
;; * yapf          -- formatter for python code

;; Emacs packages to support python development:
;; * anaconda      -- code navigation, documentation and completion
;; * blacken       -- buffer formatting on save using black
;;                    (need to pip install black)
;; * eglot         -- language server integration
;;                    (need to pip install pyright)
;; * numpydoc      -- python doc templates, uses `yasnippets'
;; * pythonic      -- utility packages for running python in different
;;                    environments (dependency of anaconda)
;; * pyvenv        -- virtualenv wrapper

;; Suggested additional keybindings for python-mode
;; (with-eval-after-load "python"
;;   (keymap-set python-mode-map "C-c C-n" #'numpydoc-generate)
;;   (keymap-set python-mode-map "C-c e n" #'flymake-goto-next-error)
;;   (keymap-set python-mode-map "C-c e p" #'flymake-goto-prev-error))

;; Suggested keybindings for pyvenv mode
;; (with-eval-after-load "pyvenv"
;;   (keymap-set pyvenv-mode-map "C-c p a" #'pyvenv-activate)
;;   (keymap-set pyvenv-mode-map "C-c p d" #'pyvenv-deactivate)
;;   (keymap-set pyvenv-mode-map "C-c p w" #'pyvenv-workon))

;;; Code:

;;; python mode
(use-package python-mode
  :straight (:type git)
  :hook (python-mode . eglot-ensure)
  :init
  (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
  :config
  (setq python-flymake-command                     '("flake8" "-")
        python-indent-trigger-commands             '(yas-expand)
        python-shell-interpreter                   "jupyter"
        python-shell-intrepreter-args              "console --simple-prompt"
        python-shell-prompt-detect-failure-warning nil
        python-shell-prompt-output-regexp          "Out\\[0-9]+\\]: "
        python-shell-prompt-regexp                 "In \\[0-9]+\\]: ")
  (customize-set-variable 'python-indent-guess-indent-offset-verbose nil))


;; edloc is built-in
(add-hook 'python-mode-hook #'eldoc-mode)


;;; anaconda
(use-package anaconda-mode
  :straight t
  :after python-mode)

;; for those who use posframe, use it to show docs
(with-eval-after-load 'posframe
  (customize-set-variable 'anaconda-mode-use-posframe-show-doc t))


;;; blacken for linting

(use-package blacken
  :straight t
  :after python-mode
  :hook python-mode)


;;; numpydoc

(use-package numpydoc
  :straight t
  :after python-mode
  :config
  (customize-set-variable 'numpydoc-insert-examples-block nil)
  (customize-set-variable 'numpydoc-template-long nil))


;;; dape for debug adapter protocol support
(with-eval-after-load 'dape
  (add-to-list 'dape-configs
               `(debugpy
                 modes (python-ts-mode python-mode)
                 command "python"
                 command-args ("-m" "debugpy.adapter")
                 :type "executable"
                 :request "launch"
                 :cwd dape-cwd-fn
                 :program dape-find-file-buffer-default)))

(provide 'custom-python-config)
;;; custom-python-config.el ends here
