(require 'cl) ; common lisp's labels is pretty great

; a list of modes which should not be considered as "code"
; used to prevent enabling of 80 column marker, line numbering, etc in modes
(setq not-code-modes-list '(eshell-mode
			    shell-mode
			    erc-mode
			    jabber-roster-mode
			    jabber-chat-mode
			    gnus-group-mode
			    gnus-summary-mode
			    gnus-article-mode))

; global key bindings
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key (kbd "C-S-<left>") 'previous-buffer)
(global-set-key (kbd "C-S-<right>") 'next-buffer)

(define-key global-map (kbd "RET") 'newline-and-indent)

(defun smart-home ()
  "Odd home to beginning of line, even home to beginning of text/code."
  (interactive)
  (if (and (eq last-command 'smart-home)
		   (/= (line-beginning-position) (point)))
	  (beginning-of-line)
    (beginning-of-line-text)))

(global-set-key [home] 'smart-home)

; load marmalade
(require 'package)
(labels ((add-repo (r)
		 (add-to-list 'package-archives r)))
  (add-repo '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-repo '("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(setq required-packages '(color-theme
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
			  haskell-mode))

; install any missing packages
(dolist (p required-packages)
  (require-package p))

(require 'ascope)

; add elisp directories to load path
(labels ((add-path (p)
		 (add-to-list 'load-path (concat "~/.emacs.d/" p))))
  (add-path "lisp") ; my elisp code
  (add-path "site-lisp") ; elisp packages not available on marmalade or melpa
  (add-path "site-lisp/p4.el")
  )

(load-library "fill-column-indicator")
(require 'p4)

; editing and navigation
(set-fill-column 80)
(turn-on-auto-fill)

(setq scroll-step 1)

; make tab less stupid
(setq tab-width 4)
(defalias 'c-basic-offset 'tab-width)

(smart-tabs-insinuate 'c)

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

; themes and display settings
(define-globalized-minor-mode global-fci-mode fci-mode
  (lambda ()
    (unless (member major-mode not-code-modes-list)
      (fci-mode t))))
(global-fci-mode t)

(setq fci-rule-width 4)

(show-paren-mode t)
(setq show-paren-delay 0)
(global-font-lock-mode t)

(load-theme 'zenburn t)

; window settings
(windmove-default-keybindings)

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
