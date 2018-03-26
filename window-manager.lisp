(in-package :liwm)

(defmacro defclass-singleton (class-name superclasses slot-names (&key variable))
  `(progn
     (defclass ,class-name ,superclasses
       ,(mapcar (lambda (slot-spec)
                  (let* ((slot-name (first slot-spec))
                         (initform-p (rest slot-spec))
                         (initform (when initform-p (second slot-spec))))
                    `(,slot-name :initarg ,(intern (princ-to-string slot-name) :keyword)
                                 ,@(when initform-p `(:initform ,initform)))))
                slot-names))
     ,@(loop :for (slot-name) :in slot-names
             :for accessor := slot-name
             :collect `(defun ,accessor ()
                         (slot-value ,variable ',slot-name))
             :collect `(defun (setf ,accessor) (,slot-name)
                         (setf (slot-value ,variable ',slot-name) ,slot-name)))))

(defvar *window-manager*)

(defclass-singleton window-manager ()
  ((display)
   (screen)
   (root)
   (windows '())
   (frame-table)
   (modifiers))
  (:variable *window-manager*))

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
  (setf (xlib:window-event-mask (root))
        '(:substructure-notify :substructure-redirect)))

(defun finalize-window-manager ()
  (ungrab-all)
  (xlib:close-display (display)))

(defun run-program (command &key wait)
  (uiop:run-program (format nil
                            "DISPLAY=~A:~D; ~{~A~^ ~}~:[~; &~]"
                            (xlib:display-host (display))
                            (xlib:display-display (display))
                            (uiop:ensure-list command)
                            wait)))
