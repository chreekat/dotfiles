(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

;; Cleanup the UI
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Dear lord, no tabs
(setq-default indent-tabs-mode nil)

;; orgmode stuff

(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c a") #'org-agenda)

; Start a capture and log buffers with god-mode disabled
(defun disable-god-mode-hook-fn ()
  "Disable god-mode in org-capture buffer."
  (when (bound-and-true-p god-local-mode)
    (god-local-mode -1)))
(add-hook 'org-capture-mode-hook #'disable-god-mode-hook-fn)
(add-hook 'org-log-buffer-setup-hook #'disable-god-mode-hook-fn)

(setq
    org-agenda-files '("~/Syncthing/PhoneFiles/org")
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
                      ("@freelance" . ?f)
                      ("@volunteering" . nil)
                      ("@housework" . ?H)
                      ("@personal" . ?p)
                      (:endgroup . nil)
                      ; non-exclusive contexts
                      ("@stackage" . ?s)
                      ("@cabal" . ?c)
                      ("@errands" . ?e)
                      ("@reading_list" . nil)
                      ("@productivity_tools" . nil)
                      ; non-exclusive attributes
                      ("pro_bono" . nil)
                      ("online" . nil)
                      ("meatspace" . nil)
                      ("easy" . nil)
                      ))

(setq org-capture-templates
      '(("i" "Inbox" entry (file "/home/b/Syncthing/PhoneFiles/org/Inbox.org"))))

(define-key global-map (kbd "<f1>")
  (lambda () (interactive) (org-capture nil "i")))

;; god-mode stuff

(use-package god-mode
    :config
    (god-mode)
    :bind (([escape] . (lambda () (interactive) (god-local-mode 1)))
           :map god-local-mode-map
           ([f1] . (lambda () (interactive) (org-capture nil "i")))
           ("i" . #'god-local-mode)
           ("." . #'repeat)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(global-auto-revert-mode t)
 '(inhibit-startup-screen t)
 '(org-agenda-files
   '("~/Syncthing/PhoneFiles/org/agenda.org" "/home/b/Syncthing/PhoneFiles/org/Projects/Stackage/Stackage.org" "/home/b/Syncthing/PhoneFiles/org/Projects/Haskell_Certification_Deployment.org" "/home/b/Syncthing/PhoneFiles/org/Inbox.org" "/home/b/Syncthing/PhoneFiles/org/Tickler.org" "/home/b/Syncthing/PhoneFiles/org/maybe.org" "/home/b/Syncthing/PhoneFiles/org/projects.org" "/home/b/Syncthing/PhoneFiles/org/reference.org" "/home/b/Syncthing/PhoneFiles/org/todos.org"))
 '(org-agenda-show-future-repeats nil)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-todo-ignore-scheduled 'all)
 '(org-capture-bookmark nil)
 '(org-export-backends '(ascii beamer html icalendar latex md odt))
 '(org-log-done 'time)
 '(org-log-into-drawer t)
 '(org-log-redeadline 'time)
 '(org-log-refile nil)
 '(org-log-reschedule 'time)
 '(org-modules
   '(ol-bbdb ol-bibtex ol-docview ol-doi ol-eww ol-gnus org-habit org-id ol-info ol-irc ol-mhe ol-rmail ol-w3m))
 '(org-priority-default 67)
 '(org-refile-targets '((org-agenda-files :maxlevel . 3)))
 '(org-refile-use-outline-path 'file)
 '(org-stuck-projects
   '("-FILE=\"/home/b/Syncthing/PhoneFiles/org/maybe.org\"+/PROJ"
     ("TODO" "OBE" "WAIT")
     nil ""))
 '(org-todo-keyword-faces
   '(("PROJ" :foreground "blue" :weight bold)
     ("WAIT" :foreground "goldenrod" :weight normal)))
 '(package-selected-packages '(god-mode use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "FSD " :family "PragmataPro Mono")))))
