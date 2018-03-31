(in-package :liwm)

(defvar *last-mouse-x* 0)
(defvar *last-mouse-y* 0)
(defvar *last-mouse-state* nil)

(defvar *event-table* (make-hash-table))

(defmacro define-event-handler (event-name keys &body body)
  (let ((fn-name (make-symbol (string event-name)))
        (event-slots (gensym)))
    `(labels ((,fn-name (&rest ,event-slots &key ,@keys &allow-other-keys)
                (declare (ignore ,event-slots))
                ,@body))
       (setf (gethash ,event-name *event-table*) #',fn-name))))

(define-event-handler :button-press (state code child x y time)
  (when child
    (let ((window (find-window child :frame t)))
      (setf *last-mouse-x* x
            *last-mouse-y* y)
      (cond ((move-mouse-input-p state code)
             (setf *last-mouse-state* :move)
             (xlib:grab-pointer child '(:pointer-motion :button-release)))
            ((resize-mouse-input-p state code)
             (setf *last-mouse-state* :resize)
             (xlib:grab-pointer child '(:pointer-motion :button-release)))
            ((left-click-input-p state code)
             (xlib:allow-events (display *window-manager*) :replay-pointer time)
             (when (on-frame-p window x y)
               (setf *last-mouse-state* :move)
               (xlib:grab-pointer child '(:pointer-motion :button-release)))))
      (when window
        (focus-window window)))))

(define-event-handler :motion-notify (event-window root-x root-y)
  (let ((mx (- root-x *last-mouse-x*))
        (my (- root-y *last-mouse-y*)))
    (let ((window (find-window event-window :frame t)))
      (case *last-mouse-state*
        (:move
         (move-window window mx my))
        (:resize
         (resize-window window mx my))))
    (setf *last-mouse-x* root-x
          *last-mouse-y* root-y)))

(define-event-handler :button-release ()
  (xlib:ungrab-pointer (display *window-manager*))
  (setf *last-mouse-state* nil))

(define-event-handler :key-press (code state)
  (alexandria:when-let (function (find-binding-key state code))
    (funcall function)))

(define-event-handler :configure-request (((:window xwin)) x y width height border-width value-mask stack-mode)
  (labels ((has-x () (= 1 (logand value-mask 1)))
           (has-y () (= 2 (logand value-mask 2)))
           (has-w () (= 4 (logand value-mask 4)))
           (has-h () (= 8 (logand value-mask 8)))
           (has-border () (= 16 (logand value-mask 16)))
           (has-stack-mode () (= 64 (logand value-mask 64))))
    (let ((window (find-window xwin :frame nil)))
      (cond (window
             (unless *last-mouse-state*
               (change-window-geometry window
                                       :x (and (has-x) x)
                                       :y (and (has-y) y)
                                       :width (and (has-w) width)
                                       :height (and (has-h) height))
               (when (has-stack-mode)
                 (focus-window window))
               (xlib:send-event xwin :configure-notify nil
                                :event-window xwin
                                :window xwin
                                :x x
                                :y y
                                :width width
                                :height height
                                :border-width border-width
                                :propagate-p nil)))
            (t
             (when (has-x) (setf (xlib:drawable-x xwin) x))
             (when (has-y) (setf (xlib:drawable-y xwin) y))
             (when (has-w) (setf (xlib:drawable-width xwin) width))
             (when (has-h) (setf (xlib:drawable-height xwin) height))
             (when (has-border) (setf (xlib:drawable-border-width xwin) border-width)))))))

(define-event-handler :map-request (window)
  (cond ((find-window window :frame nil))
        (t
         (add-window window)
         (xlib:map-window window))))

(define-event-handler :map-notify (window override-redirect-p)
  (unless override-redirect-p
    (alexandria:when-let (window (find-window window :frame nil))
      (focus-window window))))

(define-event-handler :unmap-notify (window event-window)
  (unless (xlib:window-equal (root *window-manager*) event-window)
    (alexandria:when-let (window (find-window window :frame nil))
      (if (plusp (window-count-ignore-unmap window))
          (decf (window-count-ignore-unmap window))
          (remove-window window)))))

(defun handle-event (&rest event-slots &key event-key &allow-other-keys)
  (let ((fn (gethash event-key *event-table*)))
    (when fn
      (apply fn event-slots))))
