(add-to-list 'load-path "/Users/stevechallis/.emacs.d")
(add-to-list 'load-path "/Users/stevechallis/.emacs.d/themes")
(require 'color-theme)
(require 'color-theme-tango)
(color-theme-tango)

;; Use separate directory for backups
(push '("." . "~/.emacs-backups") backup-directory-alist)

;; Hide menu bar by default
(menu-bar-mode nil)
(tool-bar-mode nil)

;; Hi ilght current line
(global-hl-line-mode 1)

;; fill wrap to ~80 chars
;; use M-q to reformat paragraph
(auto-fill-mode)

;; remap M-3 key to a hash (mac keyboards don't have has displayed)
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
(global-set-key "\C-cb" 'org-iswitchb) (global-set-key "\C-cd" 'nuke-line)
(global-set-key (kbd "C-c h") 'help-command)
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)

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
;; (define-key my-ctrl-c-map (kbd "C-c") 'eemklinks-duplicate-this-line)
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
(setq org-directory "~/org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/MobileOrg")
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/Dropbox/MobileOrg/notes.org")
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(initial-buffer-choice t)
 '(org-agenda-files (quote ("~/org/index.org" "/user/share/arcon/src/progress.org"))))

(require 'multi-term)
(setq multi-term-program "/bin/bash")
(global-set-key (kbd "C-x t") 'multi-term-next)

(defun mark-line ()
  "Highlight the whole line you are currently on"
  (interactive)
  (previous-line)
  (move-end-of-line nil)
  (set-mark-command nil) 
  (next-line)
  (move-end-of-line nil)
)