;;; custom-org-config.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: Jon Boone <ipmonger@delamancha.org>

;; Commentary

;; Provides basic configuration for Org Mode.

;;; Code:

;; Disable auto-pairing of "<" in org-mode with electric-pair-mode
(defun custom-org-enhance-electric-pair-inhibit-predicate ()
  "Disable auto-pairing of \"<\" in `org-mode' when using `electric-pair-mode'."
  (when (and electric-pair-mode (eql major-mode #'org-mode))
    (setq-local electric-pair-inhibit-predicate
                `(lambda (c)
                   (if (char-equal c ?<)
                       t
                     (,electric-pair-inhibit-predicate c))))))

;; Add hook to both electric-pair-mode-hook and org-mode-hook
;; This ensures org-mode buffers don't behave weirdly,
;; no matter when electric-pair-mode is activated.
(add-hook 'electric-pair-mode-hook #'custom-org-enhance-electric-pair-inhibit-predicate)
(add-hook 'org-mode-hook #'custom-org-enhance-electric-pair-inhibit-predicate)

;;; org-mode support
(use-package org
  :straight t
  :hook 'org-indent-mode
  :config
  ;; Return or left-click with mouse follows link
  (customize-set-variable 'org-return-follows-link t) ; return follows link
  (customize-set-variable 'org-mouse-1-follows-link t) ; left-click follows link
  (customize-set-variable 'org-link-descriptive t) ; Display link description
  (customize-set-variable 'org-hide-emphasis-markers t) ; hide markup markers
  (when (locate-library "org-appear")
    (add-hook 'org-mode-hook 'org-appear-mode)))

;;; Org-Roam Support
(use-package org-roam
  :straight t
  :bind
  (:map org-roam-mode-map
        ("C-c n l"  . org-roam-buffer-toggle)
        ("C-c n d"  . org-roam-dailies-find-date)
        ("C-c n f"  . org-roam-find-file)
        ("C-c n i"  . org-roam-node-insert))

  :config
  (setq org-roam-directory                  "~/Dropbox/pkb/org"
        org-roam-capture-templates          '(("m" "main" plain
                                               "%?"
                                               :if-new
                                               (file+head "main/${slug}.org"
                                                          "#+title: ${title\n}")
                                               :immediate-finish t
                                               :unnarrowed t)
                                              ("r" "reference" plain
                                               "%?"
                                               :if-new
                                               (file+head "reference/${title}.org"
                                                          "#+title: ${title}\n")
                                               :immediate-finish t
                                               :unnarrowed t)
                                              ("a" "article" plain
                                               "%?"
                                               :if-new
                                               (file+head "article/${title}.org"
                                                          "#+title: ${title}\n")
                                               :immediate-finish t
                                               :unnarrowed t))
        org-roam-completion-everywhere      t
        org-roam-completion-system          'default
        org-roam-dailies-capture-templates  '(("d" "default" entry
                                               #'org-roam-capture--get-point
                                               "* %?"
                                               :file-name "journal/%<%Y-%m-%d>"
                                               :head "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")
                                              ("t" "Task" entry
                                               #'org-roam-capture--get-point
                                               "* FUTURE %?\n  %U\n  %a\n  %i"
                                               :file-name "journal/%<%Y-%m-%d>"
                                               :olp ("Tasks")
                                               :empty-lines 1
                                               :head "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")
                                              ("j" "journal" entry
                                               #'org-roam-capture--get-point
                                               "* %<%I:%M %p> - Journal  :journal:\n\n%?\n\n"
                                               :file-name "journal/%<%Y-%m-%d>"
                                               :olp ("Log")
                                               :head "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")
                                              ("l" "log entry" entry
                                               #'org-roam-capture--get-point
                                               "* %<%I:%M %p> - %?"
                                               :file-name "journal/%<%Y-%m-%d>"
                                               :olp ("Log")
                                               f:head "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")
                                              ("m" "meeting" entry
                                               #'org-roam-capture--get-point
                                               "* %<%I:%M %p> - %^{Meeting Title}  :meetings:\n\n%?\n\n"
                                               :file-name "journal/%<%Y-%m-%d>"
                                               :olp ("Log")
                                               :head "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n"))
        org-roam-dailies-directory          "journal/")
  (org-roam-db-autosync-mode)
  (org-roam-setup))

(provide 'custom-org-config)
;;; custom-org-config.el ends here
