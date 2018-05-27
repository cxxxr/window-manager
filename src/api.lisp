(in-package :window-manager.api)

(defvar *mailbox* (mp:make-mailbox))

(defmacro do-wm-process (&body body)
  `(progn
     (mp:process-interrupt wm::*wm-thread*
                           (lambda () (mp:mailbox-send *mailbox* (progn ,@body))))
     (mp:mailbox-read *mailbox*)))

(defun get-window-list ()
  (do-wm-process
    (wm::vdesk-windows (wm::current-vdesk wm::*window-manager*))))

(defun get-window-name (window)
  (do-wm-process
    (wm::get-wm-name (wm::window-xwin window))))

(defun focus-window (window)
  (mp:process-interrupt wm::*wm-thread*
                        (lambda (window) (wm::focus-window window))
                        window))
