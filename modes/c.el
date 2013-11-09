(setq c-basic-offset tab-width)

(add-hook 'c-mode-common-hook
  (lambda ()
    (auto-fill-mode t)
    (set (make-local-variable 'fill-nobreak-predicate)
      (lambda ()
        (not (eq (get-text-property (point) 'face)
                 'font-lock-comment-face))))))
