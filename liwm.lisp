(in-package :liwm)

(defun event-loop ()
  (loop
   (xlib:process-event *display* :handler #'handle-event :discard-p t)))

(defun main (&key (display 0 display-p))
  (let* ((*display* (if display-p
                        (xlib:open-display "" :display display)
                        (xlib:open-default-display)))
         (*screen* (xlib:display-default-screen *display*))
         (*root* (xlib:screen-root *screen*)))
    (grab-all)
    (setf (xlib:window-event-mask *root*) '(:substructure-notify :substructure-redirect))
    (dolist (xwin (xlib:query-tree *root*))
      (when (and (eq (xlib:window-override-redirect xwin) :off)
                 (eq (xlib:window-map-state xwin) :viewable))
        (add-window xwin)))
    (unwind-protect (event-loop)
      (ungrab-all)
      (xlib:close-display *display*))))
