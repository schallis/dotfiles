;; .emacs
;;
;; Steve Challis
;; stevechallis.com
;;
;; Do not use these without understading *all* of
;; the commands, they may do unexpected things!

;; Turn on interactive do for all kinds of cool
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ;; enable fuzzy matching

;; Turn on org mode for .org files and set shortcuts
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-hook 'org-mode-hook 'turn-on-font-lock) ; not needed when global-font-lock-mode is on
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; Configure org directories
(setq org-directory "~/org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/MobileOrg")
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/Dropbox/MobileOrg/notes.org")
(custom-set-variables
 '(org-agenda-files (quote ("~/org/index.org"))))
