(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (setq indent-tabs-mode nil)         ; indent with spaces
    (setq tab-width 2)))                ; indent two columns
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)
(setq-default flyspell-issue-message-flag nil)
