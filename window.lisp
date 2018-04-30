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
   (old-x :initform nil :accessor window-old-x)
   (old-y :initform nil :accessor window-old-y)
   (old-width :initform nil :accessor window-old-width)
   (old-height :initform nil :accessor window-old-height)
   (count-ignore-unmap :initform 0 :accessor window-count-ignore-unmap)
   (fullscreen :initform nil :accessor window-fullscreen)))

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
        (set-netwm-allowed-actions xwin)
        (cond ((eq (xlib:window-map-state xwin) :viewable)
               (incf (window-count-ignore-unmap window)))
              (t
               (xlib:map-window frame)))))))

(defun remove-window (window)
  (xlib:with-server-grabbed ((display *window-manager*))
    (when (eq (current-window *window-manager*) window)
      (setf (current-window *window-manager*)
            (if (uiop:length=n-p (windows *window-manager*) 1)
                nil
                (get-previous-window window))))
    (let ((frame (window-frame window)))
      (xlib:destroy-window frame)
      (setf (windows *window-manager*)
            (delete window (windows *window-manager*))))))

(defun quit-window (window)
  (xlib:kill-client (display *window-manager*)
                    (xlib:window-id (window-xwin window))))

(defun toggle-fullscreen (window)
  (if (window-fullscreen window)
      (deactivate-fullscreen window)
      (activate-fullscreen window)))

(defun deactivate-fullscreen (window)
  (setf (window-fullscreen window) nil)
  (unmaximize-window window t))

(defun activate-fullscreen (window)
  (setf (window-fullscreen window) t)
  (maximize-window window t))

(defun maximize-window (window &optional fullscreen)
  (let (x y width height)
    (setf (window-old-x window) (window-x window)
          (window-old-y window) (window-y window)
          (window-old-width window) (window-width window)
          (window-old-height window) (window-height window)
          x (if fullscreen (- +border-width+) 0)
          y (if fullscreen (- (+ +border-width+ +frame-height+)) 0)
          width (if fullscreen
                    (xlib:drawable-width (root *window-manager*))
                    (- (xlib:drawable-width (root *window-manager*))
                       (* 2 +border-width+)))
          height (if fullscreen
                     (xlib:drawable-height (root *window-manager*))
                     (- (xlib:drawable-height (root *window-manager*))
                        (+ +frame-height+ +border-width+))))
    (xlib:change-property (window-xwin window) :_NET_WM_STATE
                          (list* (xlib:find-atom (display *window-manager*)
                                                 :_NET_WM_STATE_MAXIMIZED_VERT)
                                 (xlib:find-atom (display *window-manager*)
                                                 :_NET_WM_STATE_MAXIMIZED_HORZ)
                                 (if fullscreen
                                     (list (xlib:find-atom (display *window-manager*)
                                                           :_NET_WM_STATE_FULLSCREEN))))
                          :atom 32)
    (change-window-geometry window :x x :y y :width width :height height)))

(defun unmaximize-window (window &optional fullscreen)
  (declare (ignore fullscreen))
  (let (x y width height)
    (setf x (window-old-x window)
          y (window-old-y window)
          width (window-old-width window)
          height (window-old-height window)
          (window-old-x window) nil
          (window-old-y window) nil
          (window-old-width window) nil
          (window-old-height window) nil)
    (xlib:change-property (window-xwin window) :_NET_WM_STATE
                          '()
                          :atom 32)
    (change-window-geometry window :x x :y y :width width :height height)))

(defun toggle-maximize-window (window)
  (if (window-old-width window)
      (unmaximize-window window)
      (maximize-window window)))

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
  (unless (null (windows *window-manager*))
    (focus-window (get-next-window (current-window *window-manager*)))))

(defun focus-previous-window ()
  (unless (null (windows *window-manager*))
    (focus-window (get-previous-window (current-window *window-manager*)))))

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
