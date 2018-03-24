(in-package :liwm)

(defstruct modifiers
  meta
  alt
  super
  hyper
  numlock
  altgr)

(defvar *modifiers*)

(defun init-modifiers ()
  (labels ((find-mod (name codes)
             (find (xlib:keysym->keycodes *display* (cl-xkeysym:keysym-name->keysym name)) codes)))
    (multiple-value-bind (shift-codes lock-codes control-codes
                          mod1-codes mod2-codes mod3-codes mod4-codes mod5-codes)
        (xlib:modifier-mapping *display*)
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
        (setf *modifiers* modifiers)))))

(defun get-mods (&key meta alt super hyper numlock altgr)
  (let ((mods '()))
    (when meta
      (alexandria:nconcf mods (modifiers-meta *modifiers*)))
    (when alt
      (alexandria:nconcf mods (modifiers-alt *modifiers*)))
    (when super
      (alexandria:nconcf mods (modifiers-super *modifiers*)))
    (when hyper
      (alexandria:nconcf mods (modifiers-hyper *modifiers*)))
    (when numlock
      (alexandria:nconcf mods (modifiers-numlock *modifiers*)))
    (when altgr
      (alexandria:nconcf mods (modifiers-altgr *modifiers*)))
    mods))

(defclass input ()
  ((states :initarg :states :initform nil :reader input-states)
   (code :initarg :code :reader input-code)
   (meta :initarg :meta :reader input-meta)
   (alt :initarg :alt :reader input-alt)
   (super :initarg :super :reader input-super)
   (hyper :initarg :hyper :reader input-hyper)
   (numlock :initarg :numlock :reader input-numlock)
   (altgr :initarg :altgr :reader input-altgr)))

(defclass mouse-input (input) ())

(defun make-mouse-input (code &key meta alt super hyper numlock altgr)
  (make-instance 'mouse-input
                 :code code :meta meta :alt alt :super super
                 :hyper hyper :numlock numlock :altgr altgr))

(defun update-input-states (input)
  (with-slots (states meta alt super hyper numlock altgr) input
    (let ((mods (get-mods :meta meta :alt alt :super super :hyper hyper
                          :numlock numlock :altgr altgr)))
      (let ((s (list (apply #'xlib:make-state-mask mods)
                     (apply #'xlib:make-state-mask :lock mods)
                     (apply #'xlib:make-state-mask :mod-2 mods)
                     (apply #'xlib:make-state-mask :lock :mod-2 mods))))
        (setf states s)
        s))))

(defvar *left-click* (make-mouse-input 1 :meta t))
(defvar *right-click* (make-mouse-input 3 :meta t))

(defun input-equal (input state code)
  (and (= code (input-code input))
       (member state (input-states input))))

(defun left-click-p (state code)
  (input-equal *left-click* state code))

(defun right-click-p (state code)
  (input-equal *right-click* state code))

(defgeneric grab-input (input)
  (:method ((input mouse-input))
   (dolist (s (update-input-states input))
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

(defun init-input ()
  (init-modifiers)
  (grab-all))
