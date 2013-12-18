(setq make-command "make")
(setq num-compile-threads 5)

(defun make-dir-option (dir)
  (concat "-C " dir))

(defun make-parallel-option (num-threads)
  (concat "-j" (number-to-string num-threads)))

(defun make-command (&optional options target)
  (mapconcat 'identity
             (list make-command
                   (if (listp options)
                       (mapconcat 'identity options " ")
                     options)
                   target)
             " "))

(defun remote-command (host command)
  (concat "ssh " host " '" command "'"))

(defun make ()
  (interactive)
  (compile (make-command (make-parallel-option num-compile-threads))))

(defun make-serial ()
  (interactive)
  (compile (make-command (make-parallel-option 1))))

(defun make-clean ()
  (interactive)
  (compile (make-command nil "clean")))
            
(provide 'build)
