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

(define-event-handler :button-press (state code child x y)
  (log-format "button-press: ~A" child)
  (when child
    (cond ((move-mouse-input-p state code)
           (setf *last-mouse-x* x
                 *last-mouse-y* y
                 *last-mouse-state* :move)
           (xlib:grab-pointer child '(:pointer-motion :button-release)))
          ((resize-mouse-input-p state code)
           (setf *last-mouse-x* x
                 *last-mouse-y* y
                 *last-mouse-state* :resize)
           (xlib:grab-pointer child '(:pointer-motion :button-release))))))

(define-event-handler :motion-notify (event-window root-x root-y)
  (log-format "motion-notify: ~A" event-window)
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
  (log-format "button-release")
  (xlib:ungrab-pointer (display *window-manager*))
  (setf *last-mouse-state* nil))

(define-event-handler :key-press (code state)
  (log-format "key-press ~A ~A" code state)
  (alexandria:when-let (function (find-binding-key state code))
    (funcall function)))

(define-event-handler :configure-request (((:window xwin)) x y width height stack-mode value-mask)
  (declare (ignore stack-mode))
  (log-format "configure-request: ~A" xwin)
  (labels ((has-x () (= 1 (logand value-mask 1)))
           (has-y () (= 2 (logand value-mask 2)))
           (has-w () (= 4 (logand value-mask 4)))
           (has-h () (= 8 (logand value-mask 8))))
    (let ((window (find-window xwin :frame nil)))
      (if window
          (change-window-geometry window
                                  :x (and (has-x) x)
                                  :y (and (has-y) y)
                                  :width (and (has-w) width)
                                  :height (and (has-h) height))
          (configure-window xwin
                            :x (and (has-x) x)
                            :y (and (has-y) y)
                            :width (and (has-w) width)
                            :height (and (has-h) height))))))

(define-event-handler :map-request (window)
  (log-format "map-request: ~A" window)
  (cond ((find-window window :frame nil))
        (t
         (add-window window)
         (xlib:map-window window))))

(define-event-handler :map-notify (window override-redirect-p)
  (log-format "map-notify: ~A ~A" window override-redirect-p)
  (unless override-redirect-p
    (alexandria:when-let (window (find-window window :frame nil))
      (focus-window window))))

(define-event-handler :unmap-notify (window event-window)
  (log-format "unmap-notify: ~A ~A" window event-window)
  (unless (xlib:window-equal (root *window-manager*) event-window)
    (alexandria:when-let (window (find-window window :frame nil))
      (log-format "dec count-ignore-unmap: ~A ~A" window (window-count-ignore-unmap window))
      (if (plusp (window-count-ignore-unmap window))
          (decf (window-count-ignore-unmap window))
          (remove-window window)))))

(defun handle-event (&rest event-slots &key event-key &allow-other-keys)
  (let ((fn (gethash event-key *event-table*)))
    (when fn
      (apply fn event-slots))))
