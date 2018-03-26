(in-package :liwm)

(defvar *window-manager*)

(defclass window-manager ()
  ((display :initarg :display :reader display)
   (screen :initarg :screen :reader screen)
   (root :initarg :root :reader root)
   (windows :initform '() :accessor windows)
   (modifiers :accessor modifiers)
   (binds :initform '() :accessor binds)))

(defun make-window-manager (&optional display)
  (let* ((display (if display
                      (xlib:open-display "" :display display)
                      (xlib:open-default-display)))
         (screen (xlib:display-default-screen display))
         (root (xlib:screen-root screen)))
    (make-instance 'window-manager :display display :screen screen :root root)))

(defun initialize-window-manager ()
  (init-modifiers)
  (grab-all)
  (setf (xlib:window-event-mask (root *window-manager*))
        '(:substructure-notify :substructure-redirect)))

(defun finalize-window-manager ()
  (ungrab-all)
  (xlib:close-display (display *window-manager*)))

(defun run-program (command &key wait)
  (uiop:run-program (format nil
                            "DISPLAY=~A:~D; ~{~A~^ ~}~:[&~;~]"
                            (xlib:display-host (display *window-manager*))
                            (xlib:display-display (display *window-manager*))
                            (uiop:ensure-list command)
                            wait)))
