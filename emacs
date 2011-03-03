;; .emacs
;;
;; Steve Challis
;; stevechallis.com/emacs


(progn (cd "~/.emacs.d")
    (normal-top-level-add-subdirs-to-load-path))

(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(add-to-list 'load-path "~/.emacs.d/el-get/org-mode/lisp")

;; Use separate directory for backups
(push '("." . "~/.emacs-backups") backup-directory-alist)
(setq auto-save-file-name-transforms
       `((".*" ,"~/.emacs-tmp" t)))

;; Hide menu/scroll bar by default
(menu-bar-mode nil)
(tool-bar-mode nil)
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Highlight current line
(global-hl-line-mode 1)

;; fill wrap to ~80 chars
;; use M-q to reformat paragraph
(auto-fill-mode 1)

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (autopair-mode 1)))

(add-hook 'emacs-mode-hook
	  (lambda ()
	    (autopair-mode 1)))

(add-hook 'lisp-interaction-mode-hook
	  (lambda ()
	    (autopair-mode 1)))

(add-hook 'org-mode-hook
	  (lambda ()
	    (turn-on-auto-fill)))

;; Turn off auto wrapping
(toggle-truncate-lines nil)

;; remap M-3 key to a hash (mac keyboards don't have hash displayed)
(global-unset-key (kbd "M-3"))
(global-set-key (kbd "M-3") '(lambda() (interactive) (insert-string "#")))

;; Turn on interactive do for all kinds of cool
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ;; enable fuzzy matching

;; Turn on org mode for .org files and set shortcuts
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-hook 'org-mode-hook 'turn-on-font-lock) ; not needed when global-font-lock-mode is on
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb) (global-set-key "\C-ck" 'nuke-line)
(global-set-key (kbd "C-c h") 'help-command)
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)

;; Set up channels for sending data between emacs and a terminal
(global-set-key (kbd "C-c C-e") 'eechannel-this-line)
(global-set-key (kbd "C-c e") 'eechannel-this-line)

;; Send region to channel
(global-set-key (kbd "C-c r") 'eech)

;; Start server upon first opening emacs
(unless (= 0 (user-uid))
     (require 'server)
     (server-start))

;; Translate `C-h' to DEL (Backspace)
;; (global-set-key "\C-h" 'delete-backward-char)
;; Low-level translation should work everywhere...
(define-key key-translation-map [?\C-h] [?\C-?])

(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key "\C-x\C-k" 'kill-region)

(defun kb ()
  (kill-buffer nil)
)

;;; Use "%" to jump to the matching parenthesis.
(defun goto-match-paren (arg)
 "Go to the matching parenthesis if on parenthesis, otherwise insert
the character typed."
 (interactive "p")
 (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
       ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
       (t                    (self-insert-command (or arg 1)))))
;;(global-set-key (kbd "C-c %") 'goto-match-paren)
(global-set-key (kbd "%") 'goto-match-paren)

;; make the y or n suffice for a yes or no question
(fset 'yes-or-no-p 'y-or-n-p)

(defun open-code-line ()
 "Insert a new indented line below."
 (interactive)
 (end-of-line)
 (split-line "")
 (next-line)
 (indent-according-to-mode))

(defun open-code-line-above ()
 "Insert a new indented line above."
 (interactive)
 (beginning-of-line)
 (split-line "")
 (indent-according-to-mode))

;; Remap open-line, and use its key mapping for open-code-line.
(global-set-key "\C-co" 'open-line)
(global-set-key "\C-o" 'open-code-line)

;; Remap split-line, and use its key mapping for open-code-line-above.
(global-set-key "\C-cs" 'split-line)
(global-set-key "\C-\M-o" 'open-code-line-above)

;; Line duplication function and key binding
(defun eemklinks-duplicate-this-line ()
 "Duplicate the current line (without any changes to the kill ring)."
 (interactive)
 (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
   (save-excursion (beginning-of-line) (insert-before-markers line "\n"))))

;; (define-prefix-command 'my-ctrl-c-map)
;; (global-set-key (kbd "C-c") 'my-ctrl-c-map)

;; (define-key my-ctrl-c-map (kbd "d") 'nuke-line)
(global-set-key (kbd "C-c d") 'eemklinks-duplicate-this-line)
;; (define-key my-ctrl-c-map (kbd "h") 'help-command)

;; Define the nuke-line function. The line is killed, then the newline
;; character is deleted. The column which the cursor was positioned at
;; is then restored. Because the kill-line function is used, the
;; contents deleted can be later restored by using
;; backward-delete-char-untabifyng the yank commands.

(defun nuke-line()
 "Kill an entire line, including the trailing newline character"
 (interactive)

 ;; Store the current column position, so it can later be restored
 ;; for a more natural feel to the deletion
 (setq previous-column (current-column))

 ;; Now move to the end of the current line
 (end-of-line)

 ;; Test the length of the line. If it is 0, there is no need for a
 ;; kill-line. All that happens in this case is that the new-line
 ;; character is deleted.
 (if (= (current-column) 0)
     (delete-char 1)

     ;; This is the 'else' clause. The current line being deleted is
     ;; not zero in length. First remove the line by moving to its
     ;; start and then killing, followed by deletion of the newline
     ;; character, and then finally restoration of the column
     ;; position.
     (progn
       (beginning-of-line)
       (kill-line)
       (delete-char 1)
       (move-to-column previous-column))))

(global-set-key (kbd "M-/") 'hippie-expand)
;; (setq debug-on-error t)

;; Configure org directories
(setq org-directory "~/Dropbox/org/")

;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/mobile-org/")

;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull (concat org-mobile-directory "notes.org"))

;; Files tracked in the agenda
;; Gets confused by being a function also - using customize for now
;;(add-to-list 'org-agenda-files (concat org-directory "index.org"))
;;(add-to-list 'org-agenda-files (concat org-directory  "gtd.org"))
;;(add-to-list 'org-agenda-files (concat org-directory "notes.org"))

;; Append my own org files to the mobile-org list
;;(nconc~ org-mobile-files '("snippets.org" "shopping.org"))

;; Always open *scratch* by default
(setq initial-buffer-choice t)

;; Always want index open
(find-file (concat org-directory "/notes.org"))
(find-file (concat org-directory "/snippets.org"))

;; Set file for use by org-capture
(setq org-default-notes-file (expand-file-name "~/org/notes.org"))

(require 'el-get)
(setq el-get-sources '(session color-theme color-theme-tango-2
			       (:name multi-term 
				      :after (lambda () 
					       (setq multi-term-program "/bin/bash")
					       (global-set-key (kbd "C-x t") 'multi-term-next)))
			       ;; Does not work properly since git repo failed first time
			       ;; (:name org-mode
			       ;; 	      :after (lambda ()
			       ;; 		       (message "Requiring Org ...")
			       ;; 		       (require 'org)))
))
(el-get 'sync)


;; Not automatically done by recipe
(require 'autopair)

(require 'color-theme-tango-2)
(color-theme-tango-2)

(defun mark-line ()
  "Highlight the whole line you are currently on"
  (interactive)
  (previous-line)
  (move-end-of-line nil)
  (set-mark-command nil) 
  (next-line)
  (move-end-of-line nil)
)
;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

(setq ansi-term-color-vector [unspecified "black" "red3" "lime green"
					  "yellow3"
                                          "DeepSkyBlue3" "magenta3" "cyan3"
                                          "white"])

(setq auto-fill-mode 1)
(setq-default fill-column 80)
;; List of keys which emacs keeps for itself
;; Default: ("C-z" "C-x" "C-c" "C-h" "C-y" "<ESC>")
(setq term-unbind-key-list '("C-z" "C-x" "C-c"))

;; List of keys and functions for our use
(setq term-bind-key-alist   '(("C-c C-c" . term-interrupt-subjob)
                             ("C-c C-j" . term-line-mode)
                             ("C-c C-k" . term-char-mode)
                             ("C-c C-p" . term-previous-input)
                             ("C-c C-n" . term-next-input)
                             ("M-p" . previous-line)
                             ("M-n" . next-line)
                             ("M-s" . isearch-forward)
                             ("M-r" . isearch-backward)
                             ("C-m" . term-send-raw)
                             ("C-y" . term-paste)
                             ("M-f" . term-send-forward-word)
                             ("M-b" . term-send-backward-word)
                             ("M-o" . term-send-backspace)
                             ("C-p" . term-send-up)
                             ("C-n" . term-send-down)
                             ("M-d" . term-send-forward-kill-word)
                             ("C-w" . term-send-backward-kill-word)
                             ("C-r" . term-send-reverse-search-history)
                             ("M-," . term-send-input)
                             ("M-." . comint-dynamic-complete)))

;; Beginning of the eev block:
;; See: (find-eev "eev-rctool" "new_block_emacs")
;;      (find-eev-update-links)
;;
(add-to-list 'load-path "~/eev")
(require 'eev-all)      ; (find-eev "eev-all.el")
(eev-mode 1)
;;
;; End of the eev block.

;; (add-hook 'after-init-hook 'session-initialize)

;; save the cursor locations
(require 'saveplace)
(setq save-place-file "~/.emacs.d/cursor-places")
(setq-default save-place t)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(global-set-key (kbd "C-M-z") 'ns-toggle-fullscreen)

(require 'auto-pair+)

(require 'org)

;; Auto indent org files properly without modifying actual structure
(setq org-startup-indented t)

;; Turn on source code highlighting on org files
(setq org-src-fontify-natively t)
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

;; Add checked out docs to info path
(require 'info)
(add-to-list 'Info-default-directory-list
	     "/Users/stevechallis/.emacs.d/el-get/org-mode/doc")

(defun save-kbd-macro (name)
     "Take a name as argument and save the last defined macro under
        this name at the end of ~/.emacs.d/saved-kb-macros.el"
     (interactive "Name of the macro: ")  ; ask for the name of the macro
     (kmacro-name-last-macro name)         ; use this name for the macro
     (find-file "~/.emacs.d/saved-kb-macros.el")
     (goto-char (point-max))               ; go to the end of the file
     (newline)                             ; insert a newline
     (insert-kbd-macro name)               ; copy the macro
     (newline)                             ; insert a newline
     (save-buffer)                         ; save the edited buffer
     (switch-to-buffer nil))               ; return to the initial buffer

(global-set-key (kbd "C-c c") 'org-capture)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/Dropbox/org/gtd.org" "~/Dropbox/org/index.org" "~/Dropbox/org/notes.org"))))

(defvar org-export-docbook2twiki-xsl-file
  "docbook2twiki.xsl"
  "The full path of file docbook2twiki.xsl.
The default value is ``docbook2twiki.xsl'', which assumes that
the file exists in the current working directory, where your Org
files to be exported exist.")

;; Set org-export-docbook2twiki-xsl-file based on where you saved
;; docbook2twiki.xsl.
(setq org-export-docbook2twiki-xsl-file
      "~/org/docbook2twiki.xsl")

(defun org-export-as-twiki ()
  "Export Org file to TWiki."
  (interactive)
  (message "Exporting to TWiki...")
  (let* ((wconfig (current-window-configuration))
         (docbook-buf (org-export-as-docbook))
         (filename (buffer-file-name docbook-buf))
         (base (file-name-sans-extension filename))
         (twiki-file (concat base ".twiki")))
    (and (file-exists-p twiki-file) (delete-file twiki-file))
    (message "Processing DocBook XML file...")
    (shell-command
     (format "xsltproc --output %s %s %s"
             twiki-file
             org-export-docbook2twiki-xsl-file
             (shell-quote-argument filename)))
    (message "Processing DocBook file...done")
    (if (not (file-exists-p twiki-file))
        (error "TWiki file was not produced")
      (set-window-configuration wconfig)
      (message "Exporting to TWiki...done")
      twiki-file)))

;; Chooose which languages are enabled for inline evaluation
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . nil)
   (sh . t)))
