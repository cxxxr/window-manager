(in-package :liwm)

(defclass input ()
  ((states :initarg :states :reader input-states)
   (code :initarg :code :reader input-code)))

(defclass mouse-input (input) ())

(defun make-mouse-input (mods code)
  (let ((states (list (apply #'xlib:make-state-mask mods)
                      (apply #'xlib:make-state-mask :lock mods)
                      (apply #'xlib:make-state-mask :mod-2 mods)
                      (apply #'xlib:make-state-mask :lock :mod-2 mods))))
    (make-instance 'mouse-input :states states :code code)))

(defvar *left-click* (make-mouse-input '(:mod-1) 1))
(defvar *right-click* (make-mouse-input '(:mod-1) 3))

(defun input-equal (input state code)
  (and (= code (input-code input))
       (member state (input-states input))))

(defun left-click-p (state code)
  (input-equal *left-click* state code))

(defun right-click-p (state code)
  (input-equal *right-click* state code))

(defgeneric grab-input (input)
  (:method ((input mouse-input))
   (dolist (s (input-states input))
     (xlib:grab-button *root* (input-code input) '(:button-press) :modifiers s))))

(defgeneric ungrab-input (input)
  (:method ((input mouse-input))
   (dolist (s (input-states input))
     (xlib:ungrab-button *root* (input-code input) :modifiers s))))

(defun grab-all ()
  (grab-input *left-click*)
  (grab-input *right-click*))

(defun ungrab-all ()
  (ungrab-input *left-click*)
  (ungrab-input *right-click*))
