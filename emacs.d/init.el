(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

;; Cleanup the UI
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Dear lord, no tabs
(setq-default indent-tabs-mode nil)


(setq
    org-agenda-files '("~/Syncthing/PhoneFiles/org")
    org-startup-folded t
    fill-column 100
    )

(setq org-tag-alist '(
                      (:startgroup . nil)
                      ("@haskell_foundation" . ?h)
                      ("@stackage" . ?s)
                      ("@freelance" . ?f)
                      ("@personal_tools" . ?p)
                      ("@cabal" . ?c)
                      ("@errands" . ?e)
                      ("@pro_bono" . nil)
                      ("@housework" . ?H)
                      ("@volunteering" . nil)
                      ("@reading_list" . nil)

                      (:endgroup . nil)
                      (:startgroup . nil)
                      ("@online" . nil)
                      ("@meatspace" . nil)
                      (:endgroup . nil)
                      ("easy" . nil)
                      ))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(org-log-refile 'time)
 '(org-priority-default 67)
 '(org-refile-targets '((org-agenda-files :maxlevel . 3)))
 '(org-refile-use-outline-path 'file)
 '(org-stuck-projects
   '("+LEVEL=1+FILE=\"/home/b/Syncthing/PhoneFiles/org/projects.org\"/-DONE"
     ("TODO" "NEXT" "NEXTACTION")
     nil "")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "FSD " :family "PragmataPro Mono")))))
