(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

; delete extraneous whitespace on save
(use-package ws-butler
  :config
  (ws-butler-global-mode 1))

;; Cleanup the UI
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Dear lord, no tabs
(setq-default indent-tabs-mode nil)

;; orgmode stuff

(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c a") #'org-agenda)

; Save automatically, frequently, for SyncThing's benefit.
(add-hook 'auto-save-hook 'org-save-all-org-buffers)

; enable auto-fill-mode on org buffers
(add-hook 'org-mode-hook 'turn-on-auto-fill)

(setq
    org-startup-folded t
    fill-column 100
    )


; OBE = overcome by events
;
; I've created PROJ because I want a way to have hierarchical projects. Tagging
; a project explicitly as a *project* is my current idea for how to make that
; happen.
; WAIT exists so I know a project isn't "stuck".
(setq org-todo-keywords
      '((sequence "TODO(t)" "PROJ(p)" "WAIT(w@/!)" "|" "DONE(!)" "OBE(@)")))

(setq org-tag-alist '(
                      ; exclusive contexts
                      (:startgroup . nil)
                      ("@haskell_foundation" . ?h)
                      ("@centralapp" . ?c)
                      ("@freelance" . ?f)
                      ("@volunteering" . ?v)
                      ("@housework" . ?H)
                      ("@personal" . ?p)
                      (:endgroup . nil)
                      ; non-exclusive contexts
                      ("@errands" . ?e)
                      ; non-exclusive attributes
                      ("cabal" . nil)
                      ("stackage" . nil)
                      ("easy" . nil)
                      ("meatspace" . nil)
                      ("online" . nil)
                      ("pro_bono" . nil)
                      ("productivity_tools" . nil)
                      ("reading_list" . nil)
                      ))

(setq org-capture-templates
      '(("i" "Inbox" entry (file "/home/b/Syncthing/PhoneFiles/org/Inbox.org")
         "* %?\n[%<%Y-%m-%d %a %H:%M>]\n")))

(define-key global-map (kbd "<f1>")
  (lambda () (interactive) (org-capture nil "i")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(global-auto-revert-mode t)
 '(inhibit-startup-screen t)
 '(org-agenda-custom-commands
   '(("z" "All categories"
      ((tags-todo
        "+@haskell_foundation-secondary-SCHEDULED>\"<tomorrow>\"/TODO" nil)
       (tags-todo
        "+@centralapp-secondary-SCHEDULED>\"<tomorrow>\"/TODO" nil)
       (tags-todo "+@personal-secondary-SCHEDULED>\"<tomorrow>\"/TODO"
                  nil)
       (tags-todo
        "+@freelance-secondary-SCHEDULED>\"<tomorrow>\"/TODO" nil)
       (tags-todo
        "+@housework-secondary-SCHEDULED>\"<tomorrow>\"/TODO" nil)
       (tags-todo
        "+@volunteering-secondary-SCHEDULED>\"<tomorrow>\"/TODO" nil))
      ((org-agenda-sorting-strategy '(priority-down))))
     ("U" "Un-labeled TODOs" tags-todo "-{@.*}-secondary/TODO" nil)
     ("P" "Active projects" tags-todo "-secondary/PROJ" nil)
     ("W" "Waiting for..." tags-todo "-secondary/WAIT" nil)))
 '(org-agenda-files
   '("~/Syncthing/PhoneFiles/org/archive/2025-archive.org"
     "/home/b/Syncthing/PhoneFiles/org/books-to-read.org"
     "/home/b/Syncthing/PhoneFiles/org/thinking-about.org"
     "/home/b/Syncthing/PhoneFiles/org/maybe.org"
     "/home/b/Syncthing/PhoneFiles/org/Inbox.org"
     "/home/b/Syncthing/PhoneFiles/org/Tickler.org"
     "/home/b/Syncthing/PhoneFiles/org/agenda.org"
     "/home/b/Syncthing/PhoneFiles/org/projects.org"
     "/home/b/Syncthing/PhoneFiles/org/reference.org"
     "/home/b/Syncthing/PhoneFiles/org/todos.org"
     "/home/b/Syncthing/PhoneFiles/org/Projects/Haskell_Certification_Deployment.org"
     "/home/b/Syncthing/PhoneFiles/org/Projects/Stackage/Stackage.org"))
 '(org-agenda-show-future-repeats nil)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-sticky t)
 '(org-agenda-tags-todo-honor-ignore-options t)
 '(org-agenda-todo-ignore-scheduled 'future)
 '(org-capture-bookmark nil)
 '(org-enforce-todo-dependencies t)
 '(org-export-backends '(ascii beamer html icalendar latex md odt))
 '(org-indent-indentation-per-level 4)
 '(org-log-done 'time)
 '(org-log-into-drawer t)
 '(org-log-redeadline 'time)
 '(org-log-refile 'time)
 '(org-log-reschedule 'time)
 '(org-modules
   '(ol-bbdb ol-bibtex ol-docview ol-doi ol-eww ol-gnus org-habit org-id
             ol-info ol-irc ol-mhe ol-rmail ol-w3m))
 '(org-priority-default 67)
 '(org-refile-targets '((org-agenda-files :maxlevel . 3)))
 '(org-refile-use-outline-path 'file)
 '(org-startup-indented t)
 '(org-stuck-projects '("-secondary/PROJ" ("TODO") nil ""))
 '(org-tags-column -85)
 '(org-todo-keyword-faces
   '(("PROJ" :foreground "blue" :weight bold)
     ("WAIT" :foreground "goldenrod" :weight normal))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "FSD " :family "PragmataPro Mono")))))
