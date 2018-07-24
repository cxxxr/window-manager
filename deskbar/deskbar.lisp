(in-package :deskbar)

(defvar *deskbar*)

(capi:define-interface deskbar ()
  ((timer :initform nil :accessor deskbar-timer))
  (:panes
   (clock-pane
    capi:display-pane
    :text (current-time-string)
    :reader deskbar-clock-pane))
  (:layouts
   (top-layout
    capi:column-layout
    '(clock-pane window-list-layout))
   (window-list-layout
    capi:column-layout
    ()
    :reader deskbar-window-list-layout))
  (:default-initargs
   :best-height nil
   :best-width nil
   :layout 'top-layout
   :title "Deskbar"))

(defun start-clock (deskbar)
  (setf (deskbar-timer deskbar)
        (mp:make-timer (lambda (deskbar)
                         (capi:execute-with-interface-if-alive deskbar 'update-clock deskbar))
                       deskbar))
  (mp:schedule-timer-relative (deskbar-timer deskbar) 0.0))

(defun update-clock (deskbar)
  (setf (capi:display-pane-text (deskbar-clock-pane deskbar))
        (current-time-string))
  (mp:schedule-timer-relative (deskbar-timer deskbar) 1))

(defun deskbar ()
  (let ((deskbar (capi:display (make-instance 'deskbar))))
    (setf *deskbar* deskbar)
    (capi:execute-with-interface deskbar 'start-clock deskbar)))

(defun update-window-list ()
  (capi:apply-in-pane-process
   *deskbar*
   (lambda (panes)
     (setf (capi:layout-description (deskbar-window-list-layout *deskbar*))
           panes))
   (loop :for window :in (window-manager.api:get-window-list)
         :collect (make-instance 'capi:push-button
                                 :text (window-manager.api::get-window-name window)
                                 :external-min-width 200
                                 :data window
                                 :press-callback (lambda (window deskbar)
                                                   (declare (ignore deskbar))
                                                   (window-manager.api:focus-window window))))))

(defadvice (window-manager::change-property change-property-hook :after)
    (xwin property &rest args)
  (declare (ignore xwin args))
  (case property
    (:_NET_CLIENT_LIST
     (update-window-list))
    (:_NET_CURRENT_DESKTOP
     (update-window-list))))
