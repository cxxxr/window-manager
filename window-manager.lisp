(in-package :liwm)

(defvar *window-manager*)

(defgeneric initialize-window-manager (window-manager))
(defgeneric finalize-window-manager (window-manager))

(defclass window-manager ()
  ((display :initarg :display :reader display)
   (screen :initarg :screen :reader screen)
   (root :initarg :root :reader root)
   (windows :initform '() :accessor windows)
   (current-window :accessor current-window)
   (modifiers :accessor modifiers)
   (binds :initform '() :accessor binds)))

(defun make-window-manager (display)
  (let* ((display (if display
                      (xlib:open-display "" :display display)
                      (xlib:open-default-display)))
         (screen (xlib:display-default-screen display))
         (root (xlib:screen-root screen)))
    (make-instance 'window-manager :display display :screen screen :root root)))

(defun event-loop ()
  (loop
   (xlib:process-event (display *window-manager*)
                       :handler #'handle-event
                       :discard-p t)))

(defun run-window-manager (*window-manager*)
  (initialize-window-manager *window-manager*)
  (unwind-protect (event-loop)
    (finalize-window-manager *window-manager*)))

(defun run-program (command &key wait)
  (uiop:run-program (format nil
                            "DISPLAY=~A:~D; ~{~A~^ ~}~:[&~;~]"
                            (xlib:display-host (display *window-manager*))
                            (xlib:display-display (display *window-manager*))
                            (uiop:ensure-list command)
                            wait)))
