(in-package :window-manager)

(defvar *command-table* (make-hash-table :test 'equal))

(defmacro define-command (name &body body)
  `(setf (gethash ,name *command-table*)
         (lambda () ,@body)))

(defun get-command (name &optional (errorp t))
  (let ((fn (gethash name *command-table*)))
    (when (and errorp (not fn))
      (error "command not found: ~A" name))
    fn))

(define-command "xterm"
  (run-program "xterm" :wait nil))

(define-command "next window"
  (focus-next-window))

(define-command "previous window"
  (focus-previous-window))

(define-command "quit window"
  (alexandria:when-let (window (current-window *window-manager*))
    (quit-window window)))

(define-command "maximize window"
  (alexandria:when-let (window (current-window *window-manager*))
    (toggle-maximize-window window)))

(define-command "minimize window"
  (alexandria:when-let (window (current-window *window-manager*))
    (hide-window window)))

(macrolet ((def (n)
             `(progn
                (define-command ,(format nil "change desktop ~D" (1+ n))
                  (change-to-nth-vdesk ,n))
                (define-command ,(format nil "move window to desktop ~D" (1+ n))
                  (alexandria:when-let (window (current-window *window-manager*))
                    (move-window-to-nth-vdesk window ,n))))))
  (def 0)
  (def 1)
  (def 2)
  (def 3))
