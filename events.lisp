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
             (when (on-frame-p window x y)
               (setf *last-mouse-state* :move)
               (xlib:grab-pointer child '(:pointer-motion :button-release)))))
      (when window
        (focus-window window))))
  (xlib:allow-events (display *window-manager*) :replay-pointer time))

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
  (declare (ignore stack-mode))
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

(defun change-fullscreen (window action)
  (alexandria:switch (action :test #'=)
    (+net-wm-state-remove+
     (deactivate-fullscreen window))
    (+net-wm-state-add+
     (activate-fullscreen window))
    (+net-wm-state-toggle+
     (toggle-fullscreen window))))

(define-event-handler :client-message (((:window xwin)) type data)
  (case type
    (:_NET_CURRENT_DESKTOP
     (let ((n (aref data 0))
           (vdesks (vdesks *window-manager*)))
       (when (<= 0 n (1- (length vdesks)))
         (change-to-vdesk (elt vdesks n)))))
    (:_NET_ACTIVE_WINDOW
     (alexandria:when-let (window (find-window xwin))
       (let ((source (aref data 0)))
         (declare (ignore source))
         (focus-window window))))
    (:_NET_CLOSE_WINDOW
     (alexandria:when-let (window (find-window xwin))
       (quit-window window)))
    (:_NET_MOVERESIZE_WINDOW
     ;;TODO
     (let* ((window (find-window xwin))
            (gravity-mask (aref data 0))
            (gravity (ldb (byte 8 0) gravity-mask))
            (mask (ldb (byte 4 8) gravity-mask))
            (x (aref data 1))
            (y (aref data 2))
            (w (aref data 3))
            (h (aref data 4)))
       (declare (ignore gravity mask))
       (change-window-geometry window :x x :y y :width w :height h)))
    (:_NET_WM_DESKTOP
     (let* ((window (find-window xwin))
            (n (aref data 0))
            (vdesk (get-vdesk n)))
       (when (and window vdesk)
         ;(move-window-to-vdesk window vdesk)
         )))
    (:_NET_WM_STATE
     (alexandria:when-let (window (find-window xwin :frame nil))
       (loop :for i :from 1 :to 2
             :do (case (xlib:atom-name (display *window-manager*) (aref data i))
                   (:_NET_WM_STATE_FULLSCREEN
                    (change-fullscreen window (aref data 0)))))))))

(defun handle-event (&rest event-slots &key event-key &allow-other-keys)
  (log-format "~A" event-key)
  (let ((fn (gethash event-key *event-table*)))
    (when fn
      (apply fn event-slots))))
