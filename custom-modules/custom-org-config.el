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
  :init
  (customize-set-variable 'org-directory (expand-file-name "Dropbox/pkb/org/" (get 'abbreviated-home-dir 'home)))
  (customize-set-variable 'org-agenda-files (directory-files-recursively
                                             org-directory "\.org$"))
  (customize-set-variable 'org-return-follows-link t) ; return follows link
  (customize-set-variable 'org-mouse-1-follows-link t) ; left-click follows link
  (customize-set-variable 'org-link-descriptive t) ; Display link description
  (customize-set-variable 'org-hide-emphasis-markers t) ; hide markup markers
  :hook
  ((org-mode . org-indent-mode)
   (org-mode . org-appear-mode))
  :config
  (setq org-agenda-block-separator              "━"
        org-agenda-breadcrumbs-separator        " ❱ "
        org-agenda-category-icon-alist          '(("Work"
                                                   ,(list
                                                     (nerd-icons-faicon "cogs"))
                                                   nil nil :ascent center)
                                                  ("Personal"
                                                   ,(list
                                                     (nerd-icons-mdicon "person"))
                                                   nil nil :ascent center)
                                                  ("Calendar"
                                                   ,(list
                                                     (nerd-icons-faicon "calendar"))
                                                   nil nil :ascent center)
                                                  ("Reading"
                                                   ,(list
                                                     (nerd-icons-faicon "book"))
                                                   nil nil :ascent center))
        org-agenda-clockreport-parameter-plist  '(:link      t
                                                             :maxlevel  5
                                                             :fileskip0 t
                                                             :compact   t
                                                             :narrow    80)
        org-agenda-compact-blocks               t
        org-agenda-current-time-string          "⏰ ┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈ now"
        org-agenda-format-date                  (lambda (date)
                                                  (concat "\n"
                                                          (make-string (window-width) 9472)
                                                          "\n"
                                                          (org-agenda-format-date-aligned date)))
        org-agenda-include-deadlines            t
        org-agenda-include-diary                t
        org-agenda-prefix-format                '((agenda . "%i %-12:c%?-12t%b% s")
                                                  (todo   . "%i %-12:c")
                                                  (tags   . "%i %-12:c")
                                                  (search . "%i %-12:c"))
        org-agenda-skip-deadline-if-done        t
        org-agenda-skip-scheduled-if-done       t
        org-agenda-skip-timestamp-if-done       t
        org-agenda-skip-unavailable-files       t
        org-agenda-start-on-weekday             nil ; starts on current day
        org-agenda-time-grid                    '((weekly today require-timed)
                                                  (0800 1000 1200 1400 1600 1800 2000)
                                                  "---"
                                                  "┈┈┈┈┈┈┈┈┈┈┈┈┈┈")
        org-agenda-todo-ignore-with-date        t
        org-agenda-window-setup                 'current-window
        org-ellipsis                            " ▾"
        org-modules                             '(org-crypt
                                                  org-irc
                                                  org-habit
                                                  org-mac-iCal
                                                  org-magit
                                                  org-notify
                                                  org-toc)
        org-pretty-entities                       t
        org-tag-alist                           '((:startgroup)
                                                  ;; Put mutually exclusive tags here
                                                  (:endgroup)
                                                  ("@errand" . ?E)
                                                  ("@home" . ?H)
                                                  ("@work" . ?W)
                                                  ("agenda" . ?a)
                                                  ("planning" . ?p)
                                                  ("publish" . ?P)
                                                  ("batch" . ?b)
                                                  ("note" . ?n)
                                                  ("idea" . ?i))
        org-todo-keywords                       '((sequence
                                                   "FUTURE(f)" "ACTIVE(a)"
                                                   "|" "COMPLETED(c!)" "CANCELED(k@)"
                                                   "DEFERRED(f)")
                                                  (sequence "MEET(m)" "|" "MET(M)"))
        org-agenda-custom-commands              '(("d" "Dashboard"
                                                   ((agenda
                                                     ""
                                                     ((org-deadline-warning-days 7)))))
                                                  ("w" "Workflow Status"
                                                   ((todo
                                                     "FUTURE"
                                                     ((org-agenda-overriding-header
                                                       "To be handled in the future")
                                                      (org-agenda-files org-agenda-files)))
                                                    (todo
                                                     "ACTIVE"
                                                     ((org-agenda-overriding-header
                                                       "Active Projects")
                                                      (org-agenda-files org-agenda-files)))
                                                    (todo
                                                     "COMPLETED"
                                                     ((org-agenda-overriding-header
                                                       "Completed Projects")
                                                      (org-agenda-files org-agenda-files)))
                                                    (todo
                                                     "CANCELED"
                                                     ((org-agenda-overriding-header
                                                       "Canceled Projects")
                                                      (org-agenda-files org-agenda-files)))
                                                    (todo
                                                     "DEFERRED"
                                                     ((org-agenda-overriding-header
                                                       "Deferred Projects")
                                                      (org-agenda-files org-agenda-files)))))
                                                  ("I"
                                                   "Import diary from iCloud Calendar"
                                                   ((agenda
                                                     ""
                                                     ((org-agenda-mode-hook
                                                       (lambda ()
                                                         (org-mac-iCal))))))))))




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
