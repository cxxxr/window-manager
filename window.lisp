(in-package :liwm)

(defparameter +border-width+ 4)
(defparameter +frame-height+ 12)
(defparameter +frame-color+ #x808080)

(defun map-all-windows (function)
  (dolist (vdesk (vdesks *window-manager*))
    (dolist (window (vdesk-windows vdesk))
      (funcall function window))))

(defun find-window (xwin &key frame)
  (map-all-windows (lambda (window)
                     (when (xlib:window-equal xwin
                                              (if frame
                                                  (window-frame window)
                                                  (window-xwin window)))
                       (return-from find-window window)))))

(defun add-window (xwin)
  (xlib:with-server-grabbed ((display *window-manager*))
    (let* ((frame (xlib:create-window :parent (root *window-manager*)
                                      :x (xlib:drawable-x xwin)
                                      :y (xlib:drawable-y xwin)
                                      :width (xlib:drawable-width xwin)
                                      :height (+ (xlib:drawable-height xwin) +frame-height+)
                                      :border-width +border-width+
                                      :border +frame-color+
                                      :background +frame-color+
                                      :event-mask '(:substructure-notify
                                                    :substructure-redirect
                                                    :property-change)
                                      :override-redirect :on))
           (window (make-instance 'window
                                  :xwin xwin
                                  :frame frame
                                  :x (xlib:drawable-x xwin)
                                  :y (xlib:drawable-y xwin)
                                  :width (xlib:drawable-width xwin)
                                  :height (xlib:drawable-height xwin)))
           (current-vdesk (current-vdesk *window-manager*)))
      (push window (vdesk-windows current-vdesk))
      (set-net-wm-desktop window current-vdesk)
      (set-net-client-list (vdesk-windows current-vdesk))
      (set-net-client-list-stacking (vdesk-windows current-vdesk))
      (xlib:add-to-save-set xwin)
      (xlib:reparent-window xwin frame 0 +frame-height+)
      (set-net-wm-allowed-actions xwin)
      (cond ((eq (xlib:window-map-state xwin) :viewable)
             (incf (window-count-ignore-unmap window)))
            (t
             (xlib:map-window frame))))))

(defun remove-window (window)
  (xlib:with-server-grabbed ((display *window-manager*))
    (when (eq (current-window *window-manager*) window)
      (focus-window (if (uiop:length=n-p (vdesk-windows (current-vdesk *window-manager*)) 1)
                        nil
                        (get-previous-window window))))
    (let ((frame (window-frame window)))
      (xlib:destroy-window frame)
      (remove-vdesk-window window)
      (set-net-client-list (vdesk-windows (current-vdesk *window-manager*)))
      (set-net-client-list-stacking (vdesk-windows (current-vdesk *window-manager*))))))

(defun hide-window (window)
  (unless (eq (xlib:window-map-state (window-xwin window)) :unmapped)
    ;(incf (window-count-ignore-unmap window))
    (setf (wm-state (window-xwin window)) +iconic-state+)
    (xlib:unmap-window (window-frame window))))

(defun show-window (window)
  (xlib:map-window (window-frame window))
  (setf (wm-state (window-xwin window)) +normal-state+))

(defun quit-window (window)
  (send-client-message (window-xwin window)
                       :WM_PROTOCOLS
                       (xlib:intern-atom (display *window-manager*) :WM_DELETE_WINDOW)))

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

(defun update-window-order ()
  (let ((pos (position (current-window *window-manager*) (vdesk-windows (current-vdesk *window-manager*)))))
    (set-net-client-list-stacking
     (setf (vdesk-windows (current-vdesk *window-manager*))
           (nconc (subseq (vdesk-windows (current-vdesk *window-manager*)) pos)
                  (subseq (vdesk-windows (current-vdesk *window-manager*)) 0 pos))))))

(defun focus-window (window)
  (setf (current-window *window-manager*) window)
  (when window
    (xlib:set-input-focus (display *window-manager*) (window-xwin window) :pointer-root)
    (update-window-order)
    (let ((xwin (window-frame window)))
      (setf (xlib:window-priority xwin) :above)
      (dolist (w (xlib:query-tree (root *window-manager*)))
        (dolist (id (xlib:get-property xwin :WM_TRANSIENT_FOR))
          (when (= id (xlib:window-id xwin))
            (setf (xlib:window-priority w) :above)))))))

(defun get-next-window (window)
  (next-element window (vdesk-windows (current-vdesk *window-manager*))))

(defun get-previous-window (window)
  (previous-element window (vdesk-windows (current-vdesk *window-manager*))))

(defun focus-next-window ()
  (unless (null (vdesk-windows (current-vdesk *window-manager*)))
    (focus-window (get-next-window (current-window *window-manager*)))))

(defun focus-previous-window ()
  (unless (null (vdesk-windows (current-vdesk *window-manager*)))
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
