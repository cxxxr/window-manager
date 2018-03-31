(in-package :liwm)

(defparameter +border-width+ 4)
(defparameter +frame-height+ 12)
(defparameter +frame-color+ #x808080)

(defclass window ()
  ((xwin :initarg :xwin :reader window-xwin)
   (frame :initarg :frame :reader window-frame)
   (x :initarg :x :accessor window-x)
   (y :initarg :y :accessor window-y)
   (width :initarg :width :accessor window-width)
   (height :initarg :height :accessor window-height)
   (count-ignore-unmap :initform 0 :accessor window-count-ignore-unmap)))

(defun find-window (xwin &key frame)
  (if frame
      (find xwin (windows *window-manager*) :key #'window-frame :test #'xlib:window-equal)
      (find xwin (windows *window-manager*) :key #'window-xwin :test #'xlib:window-equal)))

(defun add-window (xwin)
  (xlib:with-server-grabbed ((display *window-manager*))
    (let ((frame (xlib:create-window :parent (root *window-manager*)
                                     :x (xlib:drawable-x xwin)
                                     :y (xlib:drawable-y xwin)
                                     :width (xlib:drawable-width xwin)
                                     :height (+ (xlib:drawable-height xwin) +frame-height+)
                                     :border-width +border-width+
                                     :border +frame-color+
                                     :background +frame-color+
                                     :event-mask '(:substructure-notify
                                                   :substructure-redirect)
                                     :override-redirect :on)))
      (log-format "frame:~A child:~A" frame xwin)
      (let ((window (make-instance 'window
                                   :xwin xwin
                                   :frame frame
                                   :x (xlib:drawable-x xwin)
                                   :y (xlib:drawable-y xwin)
                                   :width (xlib:drawable-width xwin)
                                   :height (xlib:drawable-height xwin))))
        (push window (windows *window-manager*))
        (xlib:add-to-save-set xwin)
        (xlib:reparent-window xwin frame 0 +frame-height+)
        (cond ((eq (xlib:window-map-state xwin) :viewable)
               (log-format "inc count-ignore-unmap: ~A ~A"
                           window (window-count-ignore-unmap window))
               (incf (window-count-ignore-unmap window)))
              (t
               (xlib:map-window frame)))))))

(defun remove-window (window)
  (log-format "remove-window: ~A" window)
  (xlib:with-server-grabbed ((display *window-manager*))
    (let ((frame (window-frame window))
          ;(xwin (window-xwin window))
          )
      ;(xlib:reparent-window xwin (root *window-manager*) 0 0)
      ;(xlib:remove-from-save-set xwin)
      ;(xlib:unmap-window frame)
      (xlib:destroy-window frame)
      (setf (windows *window-manager*)
            (delete window (windows *window-manager*))))))

(defun on-frame-p (window x y)
  (declare (ignore x))
  (<= 0 (- y (window-y window)) +frame-height+))

(defun focus-window (window)
  (xlib:set-input-focus (display *window-manager*) (window-xwin window) :pointer-root)
  (setf (current-window *window-manager*) window)
  (let ((xwin (window-frame window)))
    (setf (xlib:window-priority xwin) :above)
    (dolist (w (xlib:query-tree (root *window-manager*)))
      (dolist (id (xlib:get-property xwin :WM_TRANSIENT_FOR))
        (when (= id (xlib:window-id xwin))
          (setf (xlib:window-priority w) :above))))))

(defun get-next-window (window)
  (let ((windows (windows *window-manager*)))
    (or (second (member window windows))
        (first windows))))

(defun get-previous-window (window)
  (let ((windows (windows *window-manager*)))
    (if (eq window (first windows))
        (alexandria:lastcar windows)
        (loop :for rest :on windows
              :do (when (eq window (second rest))
                    (return (first rest)))))))

(defun focus-next-window ()
  (focus-window (get-next-window (current-window *window-manager*))))

(defun focus-previous-window ()
  (focus-window (get-previous-window (current-window *window-manager*))))

(defun move-window (window mx my)
  (change-window-geometry window
                          :x (+ (window-x window) mx)
                          :y (+ (window-y window) my)))

(defun resize-window (window mx my)
  (change-window-geometry window
                          :width (+ (window-width window) mx)
                          :height (+ (window-height window) my)))

(defun change-window-geometry (window &key x y width height (border +border-width+))
  (let ((xwin (window-xwin window))
        (frame (window-frame window)))
    (xlib:with-state (frame)
      (xlib:with-state (xwin)
        (when x
          (setf (window-x window) x)
          (setf (xlib:drawable-x frame) x))
        (when y
          (setf (window-y window) y)
          (setf (xlib:drawable-y frame) y))
        (when width
          (setf (xlib:drawable-width frame) (+ (xlib:drawable-x xwin) width border))
          (setf (window-width window) width)
          (setf (xlib:drawable-width xwin) width))
        (when height
          (setf (xlib:drawable-height frame) (+ (xlib:drawable-y xwin) height border))
          (setf (window-height window) height)
          (setf (xlib:drawable-height xwin) height))))))
