; remove cl deprecation warnings from Emacs > 24.3
(if (not (version< emacs-version "24.3"))
    (require 'cl-lib)
    (progn
      (require 'cl)
      (defalias 'cl-labels 'labels)))

; a list of modes which should not be considered as "code"
; used to prevent enabling of 80 column marker, line numbering, etc in modes
(setq not-code-modes-list '(dired-mode
                            eshell-mode
                            shell-mode
                            erc-mode
                            jabber-roster-mode
                            jabber-chat-mode
                            gnus-group-mode
                            gnus-summary-mode
                            gnus-article-mode))

; add elisp directories to load path
; TODO: go look up the way to do this recursively to avoid site-lisp/foo
(cl-labels ((add-path (p)
              (add-to-list 'load-path (concat "~/.emacs.d/" p))))
  (add-path "")                         ; allow elisp in the main directory
  (add-path "lisp")                     ; my elisp code
  (add-path "site-lisp"))               ; elisp packages not available on repos

(require 'packages)
(pkg-require '(color-theme
               zenburn-theme
               cl-lib
               smart-tabs-mode
               clojure-mode
               ascope
               fuzzy-format
               git-commit-mode
               git-rebase-mode
               gitconfig-mode
               gitignore-mode
               magit
               p4
               haskell-mode))

(require 'keybindings)
(require 'ascope)
(require 'p4)

(load-library "fill-column-indicator")

(defun require-dir (dir)
  (interactive "Directory: ")
  (dolist (file (directory-files dir t))
    (when (file-regular-p file)
      (load-file file))))

; make tab less stupid
(setq-default tab-width 4)

; Align with spaces only
(defadvice align (around align-with-spaces)
  "Never use tabs for alignment."
  (let ((indent-tabs-mode nil))
    ad-do-it))
(ad-activate 'align)

(defadvice align-regexp (around align-regexp-with-spaces)
  "Never use tabs for alignment."
  (let ((indent-tabs-mode nil))
    ad-do-it))
(ad-activate 'align-regexp)

; themes and display settings
(define-globalized-minor-mode global-fci-mode fci-mode
  (lambda ()
    (unless (member major-mode not-code-modes-list)
      (fci-mode t))))
(global-fci-mode t)

(setq fci-rule-width 4)

; default character color won't display with zenburn in text mode
(setq fci-rule-character-color "white")

; editing and navigation
(setq-default fill-column 80)

(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

(show-paren-mode t)
(setq show-paren-delay 0)
(global-font-lock-mode t)

(load-theme 'zenburn t)

; window settings
(windmove-default-keybindings 'meta)

(defadvice linum-on (around linum-on-inhibit-for-modes)
  "Stop the load of linum-mode for some major modes."
  (unless (member major-mode not-code-modes-list)
    ad-do-it))

(ad-activate 'linum-on)
(global-linum-mode)

(line-number-mode t)
(column-number-mode t)

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(setq inhibit-splash-screen t)

(require-dir "~/.emacs.d/modes")

; include local configuration last so it may override the above settings
(require 'local-config)
