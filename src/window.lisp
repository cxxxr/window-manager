(in-package :window-manager)

(defparameter +border-width+ 4)
(defparameter +frame-height+ 12)
(defparameter +frame-color+ #x808080)

(defun find-window (xwin &key frame)
  (find xwin (all-windows *window-manager*)
        :key (if frame #'window-frame #'window-xwin)
        :test #'xlib:window-equal))

(defun all-ordered-windows ()
  (let ((current-vdesk (current-vdesk *window-manager*)))
    (loop :for vdesk :in (cons current-vdesk (remove current-vdesk (vdesks *window-manager*)))
          :append (vdesk-windows vdesk))))

(defstruct (frame-extents (:type list) (:constructor %make-frame-extents (left right top bottom)))
  left right top bottom)

(defun make-frame-extents (use-decoration)
  (if use-decoration
      (%make-frame-extents +border-width+ +border-width+ +frame-height+ +border-width+)
      (%make-frame-extents 0 0 0 0)))

(defun decoration-from-mwm-hints (xwin)
  (let ((result (xlib:get-property xwin :_MOTIF_WM_HINTS)))
    (or (null result)
        (destructuring-bind (flags functions decorations input-mode status)
            result
          (declare (ignore functions input-mode status))
          (not (and (logand flags 2)
                    (zerop (logand decorations 1))
                    (zerop (logand decorations 2))))))))

(defun add-window (xwin)
  (log-format "add-window: ~A type=~A" xwin (get-net-window-type xwin))
  (when (eq (xlib:window-override-redirect xwin) :on)
    (return-from add-window))
  (when (dock-p xwin)
    (return-from add-window))
  (xlib:with-server-grabbed ((display *window-manager*))
    (let* ((use-decoration (decoration-from-mwm-hints xwin))
           (frame-extents (make-frame-extents use-decoration))
           (frame (xlib:create-window :parent (root *window-manager*)
                                      :x (xlib:drawable-x xwin)
                                      :y (xlib:drawable-y xwin)
                                      :width (xlib:drawable-width xwin)
                                      :height (+ (xlib:drawable-height xwin)
                                                 (+ (frame-extents-top frame-extents)
                                                    (frame-extents-bottom frame-extents)))
                                      :border-width (frame-extents-left frame-extents)
                                      :border +frame-color+
                                      :background +frame-color+
                                      :event-mask '(:substructure-notify
                                                    :substructure-redirect)
                                      :override-redirect :on))
           (window (make-instance 'window
                                  :xwin xwin
                                  :frame frame
                                  :x (xlib:drawable-x xwin)
                                  :y (xlib:drawable-y xwin)
                                  :width (xlib:drawable-width xwin)
                                  :height (xlib:drawable-height xwin)
                                  :frame-extents frame-extents))
           (current-vdesk (current-vdesk *window-manager*)))
      (push window (vdesk-windows current-vdesk))
      (alexandria:nconcf (all-windows *window-manager*) (list window))
      (set-net-wm-desktop window current-vdesk)
      (set-net-frame-extents (window-xwin window) frame-extents)
      (update-net-client-list)
      (update-net-client-list-stacking)
      (xlib:add-to-save-set xwin)
      (xlib:reparent-window xwin frame 0 (frame-extents-top frame-extents))
      (init-net-wm-allowed-actions xwin)
      (cond ((eq (xlib:window-map-state xwin) :viewable)
             (incf (window-count-ignore-unmap window)))
            (t
             (xlib:map-window frame)))
      window)))

(defun remove-window (window)
  (log-format "remove-window: ~A" window)
  (xlib:with-server-grabbed ((display *window-manager*))
    (when (eq (current-window *window-manager*) window)
      (setf (current-window *window-manager*) nil))
    (xlib:destroy-window (window-frame window))
    (remove-vdesk-window window)
    (alexandria:deletef (all-windows *window-manager*) window)
    (remove-net-wm-desktop (window-xwin window))
    (update-net-client-list)
    (update-net-client-list-stacking)))

(defun hide-window-xwin (window)
  (unless (eq (xlib:window-map-state (window-xwin window)) :unmapped)
    ;(incf (window-count-ignore-unmap window))
    (setf (wm-state (window-xwin window)) +iconic-state+)
    (xlib:unmap-window (window-frame window))))

(defun hide-window (window)
  (unless (window-hidden-p window)
    (setf (window-hidden-p window) t)
    (let ((vdesk (find-vdesk-from-window window)))
      (let ((pos (position window (vdesk-windows vdesk))))
        (setf (vdesk-windows vdesk)
              (nconc (subseq (vdesk-windows vdesk) 0 pos)
                     (subseq (vdesk-windows vdesk) (1+ pos))
                     (list window))))
      (update-net-client-list-stacking)
      (when (eq window (current-window *window-manager*))
        (setf (current-window *window-manager*) nil))
      (when (eq vdesk (current-vdesk *window-manager*))
        (hide-window-xwin window)))))

(defun show-window-xwin (window)
  (xlib:map-window (window-frame window))
  (setf (wm-state (window-xwin window)) +normal-state+))

(defun show-window (window)
  (setf (window-hidden-p window) nil)
  (when (eq (find-vdesk-from-window window)
            (current-vdesk *window-manager*))
    (show-window-xwin window)))

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
  (let ((frame-extents (window-frame-extents window))
        x y width height)
    (setf (window-old-x window) (window-x window)
          (window-old-y window) (window-y window)
          (window-old-width window) (window-width window)
          (window-old-height window) (window-height window)
          x (if fullscreen (- (frame-extents-left frame-extents)) 0)
          y (if fullscreen
                (- (+ (frame-extents-top frame-extents)
                      (frame-extents-bottom frame-extents)))
                0)
          width (if fullscreen
                    (xlib:drawable-width (root *window-manager*))
                    (- (xlib:drawable-width (root *window-manager*))
                       (+ (frame-extents-left frame-extents)
                          (frame-extents-right frame-extents))))
          height (if fullscreen
                     (xlib:drawable-height (root *window-manager*))
                     (- (xlib:drawable-height (root *window-manager*))
                        (+ (frame-extents-top frame-extents)
                           (frame-extents-bottom frame-extents)))))
    (add-net-wm-state (window-xwin window)
                      (list* :_NET_WM_STATE_MAXIMIZED_VERT
                             :_NET_WM_STATE_MAXIMIZED_HORZ
                             (and fullscreen (list :_NET_WM_STATE_FULLSCREEN))))
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
    (remove-net-wm-state (window-xwin window)
                         '(:_NET_WM_STATE_MAXIMIZED_VERT
                           :_NET_WM_STATE_MAXIMIZED_HORZ
                           :_NET_WM_STATE_FULLSCREEN))
    (change-window-geometry window :x x :y y :width width :height height)))

(defun toggle-maximize-window (window)
  (if (window-old-width window)
      (unmaximize-window window)
      (maximize-window window)))

(defun on-frame-p (window x y)
  (declare (ignore x))
  (<= 0
      (- y (window-y window))
      (frame-extents-top (window-frame-extents window))))

(defun update-window-order ()
  (let ((pos (position (current-window *window-manager*) (vdesk-windows (current-vdesk *window-manager*)))))
    (setf (vdesk-windows (current-vdesk *window-manager*))
          (nconc (subseq (vdesk-windows (current-vdesk *window-manager*)) pos)
                 (subseq (vdesk-windows (current-vdesk *window-manager*)) 0 pos))))
  (update-net-client-list-stacking))

(defun focus-window (window)
  (log-format "focus-window: ~A" window)
  (unless (eq (current-window *window-manager*) window)
    (setf (current-window *window-manager*) window)
    (when window
      (when (window-hidden-p window)
        (show-window window))
      (xlib:set-input-focus (display *window-manager*) (window-xwin window) :pointer-root)
      (update-window-order)
      (let ((xwin (window-frame window)))
        (setf (xlib:window-priority xwin) :above)
        (dolist (w (xlib:query-tree (root *window-manager*)))
          (dolist (id (xlib:get-property xwin :WM_TRANSIENT_FOR))
            (when (= id (xlib:window-id xwin))
              (setf (xlib:window-priority w) :above))))))))

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

(defun change-window-geometry (window &key x y width height)
  (let ((xwin (window-xwin window))
        (frame (window-frame window))
        (frame-extents (window-frame-extents window)))
    (xlib:with-state (frame)
      (xlib:with-state (xwin)
        (when x
          (setf (window-x window) x)
          (setf (xlib:drawable-x frame) x))
        (when y
          (setf (window-y window) y)
          (setf (xlib:drawable-y frame) y))
        (when width
          (setf (xlib:drawable-width frame)
                (+ (xlib:drawable-x xwin) width (frame-extents-right frame-extents)))
          (setf (window-width window) width)
          (setf (xlib:drawable-width xwin) width))
        (when height
          (setf (xlib:drawable-height frame)
                (+ (xlib:drawable-y xwin) height (frame-extents-bottom frame-extents)))
          (setf (window-height window) height)
          (setf (xlib:drawable-height xwin) height))))))
