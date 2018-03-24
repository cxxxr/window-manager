(in-package :liwm)

(defparameter *window-list* '())
(defparameter *frame-table* (make-hash-table))

(defclass window ()
  ((xwin :initarg :xwin :reader window-xwin)
   (frame :initarg :frame :reader window-frame)
   (x :initarg :x :accessor window-x)
   (y :initarg :y :accessor window-y)
   (width :initarg :width :accessor window-width)
   (height :initarg :height :accessor window-height)
   (count-ignore-unmap :initform 0 :accessor window-count-ignore-unmap)))

(defun init-window ()
  (setf *window-list* '())
  (setf *frame-table* (make-hash-table)))

(defun find-window (xwin &key frame)
  (if frame
      (find xwin *window-list* :key #'window-frame :test #'xlib:window-equal)
      (find xwin *window-list* :key #'window-xwin :test #'xlib:window-equal)))

(defun add-window (xwin)
  (xlib:with-server-grabbed (*display*)
    (let ((frame (xlib:create-window :parent *root*
                                     :x (xlib:drawable-x xwin)
                                     :y (xlib:drawable-y xwin)
                                     :width (xlib:drawable-width xwin)
                                     :height (xlib:drawable-height xwin)
                                     :border-width 2
                                     :border #xff0000
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
        (push window *window-list*)
        (xlib:add-to-save-set xwin)
        (xlib:reparent-window xwin frame 0 0)
        (cond ((eq (xlib:window-map-state xwin) :viewable)
               (log-format "inc count-ignore-unmap: ~A ~A" window (window-count-ignore-unmap window))
               (incf (window-count-ignore-unmap window)))
              (t
               (xlib:map-window frame)))))))

(defun remove-window (window)
  (log-format "remove-window: ~A" window)
  (xlib:with-server-grabbed (*display*)
    (let ((frame (window-frame window))
          (xwin (window-xwin window)))
      ;(xlib:reparent-window xwin *root* 0 0)
      ;(xlib:remove-from-save-set xwin)
      ;(xlib:unmap-window frame)
      (xlib:destroy-window frame)
      (setf *window-list* (delete window *window-list*)))))

(defun move-window (window mx my)
  (let ((frame (window-frame window)))
    (xlib:with-state (frame)
      (incf (xlib:drawable-x frame) mx)
      (incf (xlib:drawable-y frame) my))
    (setf (window-x window) (xlib:drawable-x frame))
    (setf (window-y window) (xlib:drawable-y frame))))

(defun resize-window (window mx my)
  (let ((xwin (window-xwin window))
        (frame (window-frame window)))
    (xlib:with-state (frame)
      (incf (xlib:drawable-width frame) mx)
      (incf (xlib:drawable-height frame) my)
      (xlib:with-state (xwin)
        (setf (xlib:drawable-width xwin) (xlib:drawable-width frame))
        (setf (xlib:drawable-height xwin) (xlib:drawable-height frame))))
    (setf (window-width window) (xlib:drawable-width xwin))
    (setf (window-height window) (xlib:drawable-height xwin))))

(defun change-window-geometry (window &key x y width height)
  (labels ((configure (xwin)
             (configure-window xwin :x x :y y :width width :height height)))
    (let ((xwin (window-xwin window))
          (frame (window-frame window)))
      (xlib:with-state (frame)
        (configure frame)
        (xlib:with-state (xwin)
          (configure xwin))))))