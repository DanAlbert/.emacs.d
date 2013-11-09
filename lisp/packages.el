(if (not (version< emacs-version "24.3"))
    (require 'cl-lib)
    (progn
      (require 'cl)
      (defalias 'cl-labels 'labels)))

; load ELPA repos
(require 'package)
(cl-labels ((add-repo (r)
              (add-to-list 'package-archives r)))
  (add-repo '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-repo '("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

; refresh package lists
(defun pkg-refresh ()
  (when (not package-archive-contents)
    (package-refresh-contents)))

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

; install any missing packages
(defun pkg-require (required-packages)
  (dolist (p required-packages)
    (require-package p)))

(provide 'packages)
