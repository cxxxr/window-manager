(in-package :liwm)

(defstruct modifiers
  meta
  alt
  super
  hyper
  numlock
  altgr)

(defun init-modifiers ()
  (labels ((find-mod (name codes)
             (find (xlib:keysym->keycodes (display *window-manager*)
                                          (cl-xkeysym:keysym-name->keysym name))
                   codes)))
    (multiple-value-bind (shift-codes lock-codes control-codes
                          mod1-codes mod2-codes mod3-codes mod4-codes mod5-codes)
        (xlib:modifier-mapping (display *window-manager*))
      (declare (ignore shift-codes lock-codes control-codes))
      (let ((modifiers (make-modifiers)))
        (loop :for mod :in '(:mod-1 :mod-2 :mod-3 :mod-4 :mod-5)
              :for codes :in (list mod1-codes mod2-codes mod3-codes mod4-codes mod5-codes)
              :do (cond ((or (find-mod "Meta_L" codes)
                             (find-mod "Meta_R" codes))
                         (push mod (modifiers-meta modifiers)))
                        ((or (find-mod "Alt_L" codes)
                             (find-mod "Alt_R" codes))
                         (push mod (modifiers-alt modifiers)))
                        ((or (find-mod "Super_L" codes)
                             (find-mod "Super_R" codes))
                         (push mod (modifiers-super modifiers)))
                        ((or (find-mod "Hyper_L" codes)
                             (find-mod "Hyper_R" codes))
                         (push mod (modifiers-hyper modifiers)))
                        ((find-mod "Num_Lock" codes)
                         (push mod (modifiers-numlock modifiers)))
                        ((find-mod "ISO_Level3_Shift" codes)
                         (push mod (modifiers-altgr modifiers)))))
        (setf (modifiers *window-manager*) modifiers)))))

(defun get-mods (&key control meta alt super hyper numlock altgr)
  (let ((mods '()))
    (when control
      (push :control mods))
    (when meta
      (alexandria:nconcf mods (modifiers-meta (modifiers *window-manager*))))
    (when alt
      (alexandria:nconcf mods (modifiers-alt (modifiers *window-manager*))))
    (when super
      (alexandria:nconcf mods (modifiers-super (modifiers *window-manager*))))
    (when hyper
      (alexandria:nconcf mods (modifiers-hyper (modifiers *window-manager*))))
    (when numlock
      (alexandria:nconcf mods (modifiers-numlock (modifiers *window-manager*))))
    (when altgr
      (alexandria:nconcf mods (modifiers-altgr (modifiers *window-manager*))))
    mods))

(defclass input ()
  ((states :initarg :states :initform nil :accessor input-states)
   (code :initarg :code :accessor input-code)
   (control :initarg :control :reader input-control)
   (meta :initarg :meta :reader input-meta)
   (alt :initarg :alt :reader input-alt)
   (super :initarg :super :reader input-super)
   (hyper :initarg :hyper :reader input-hyper)
   (numlock :initarg :numlock :reader input-numlock)
   (altgr :initarg :altgr :reader input-altgr)))

(defclass key-input (input)
  ((name :initarg :name :reader key-input-name)))

(defclass mouse-input (input)
  ())

(defun make-key-input (name &key control meta alt super hyper numlock altgr)
  (make-instance 'key-input
                 :name name :control control :meta meta :alt alt :super super
                 :hyper hyper :numlock numlock :altgr altgr))

(defun make-mouse-input (code &key control meta alt super hyper numlock altgr)
  (make-instance 'mouse-input
                 :code code :control control :meta meta :alt alt :super super
                 :hyper hyper :numlock numlock :altgr altgr))

(defun update-input-states (input)
  (let ((mods (get-mods :control (input-control input)
                        :meta (input-meta input)
                        :alt (input-alt input)
                        :super (input-super input)
                        :hyper (input-hyper input)
                        :numlock (input-numlock input)
                        :altgr (input-altgr input))))
    (let ((status (list (apply #'xlib:make-state-mask mods)
                        (apply #'xlib:make-state-mask :lock mods)
                        (apply #'xlib:make-state-mask :mod-2 mods)
                        (apply #'xlib:make-state-mask :lock :mod-2 mods))))
      (setf (input-states input) status)
      status)))

(defparameter *move-mouse-input* (make-mouse-input 1 :meta t))
(defparameter *resize-mouse-input* (make-mouse-input 3 :meta t))
(defparameter *left-click-input* (make-mouse-input 1))

(defun input-equal (input state code)
  (and (= code (input-code input))
       (member state (input-states input))))

(defun move-mouse-input-p (state code)
  (input-equal *move-mouse-input* state code))

(defun resize-mouse-input-p (state code)
  (input-equal *resize-mouse-input* state code))

(defun left-click-input-p (state code)
  (input-equal *left-click-input* state code))

(defgeneric grab-input (input)
  (:method ((input mouse-input))
   (dolist (s (update-input-states input))
     (xlib:grab-button (root *window-manager*)
                       (input-code input)
                       '(:button-press)
                       :modifiers s
                       :owner-p nil
                       :sync-pointer-p t
                       :sync-keyboard-p nil)))
  (:method ((input key-input))
   (let ((code (xlib:keysym->keycodes (display *window-manager*)
                                      (cl-xkeysym:keysym-name->keysym
                                       (key-input-name input)))))
     (setf (input-code input) code)
     (dolist (s (update-input-states input))
       (xlib:grab-key (root *window-manager*) code :modifiers s)))))

(defgeneric ungrab-input (input)
  (:method ((input mouse-input))
   (dolist (s (input-states input))
     (xlib:ungrab-button (root *window-manager*) (input-code input) :modifiers s)))
  (:method ((input key-input))
   (dolist (s (input-states input))
     (xlib:ungrab-key (root *window-manager*) (input-code input) :modifiers s))))

(defun grab-all ()
  (grab-input *move-mouse-input*)
  (grab-input *resize-mouse-input*)
  (grab-input *left-click-input*))

(defun ungrab-all ()
  (ungrab-input *move-mouse-input*)
  (ungrab-input *resize-mouse-input*)
  (ungrab-input *left-click-input*)
  (loop :for (key) :in (binds *window-manager*)
        :do (ungrab-input key)))

(defun bind-key (key function)
  (grab-input key)
  (push (cons key function) (binds *window-manager*)))

(defun find-binding-key (state code)
  (loop :for (key . function) :in (binds *window-manager*)
        :when (input-equal key state code)
        :do (return function)))
