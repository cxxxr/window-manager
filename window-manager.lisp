(in-package :liwm)

(defvar *window-manager*)

(defparameter *root-window-properties*
  '(:_NET_CLIENT_LIST
    :_NET_CLIENT_LIST_STACKING
    :_NET_NUMBER_OF_DESKTOPS ;*
    :_NET_DESKTOP_GEOMETRY
    :_NET_DESKTOP_VIEWPORT
    :_NET_CURRENT_DESKTOP
    ;:_NET_DESKTOP_NAMES
    :_NET_ACTIVE_WINDOW
    :_NET_WORKAREA
    :_NET_SUPPORTING_WM_CHECK
    ;:_NET_VIRTUAL_ROOTS
    ;:_NET_DESKTOP_LAYOUT
    ;:_NET_SHOWING_DESKTOP
    ))

(defparameter *other-root-window-messages*
  '(:_NET_CLOSE_WINDOW
    :_NET_MOVERESIZE_WINDOW ;*
    ;:_NET_WM_MOVERESIZE
    ;:_NET_RESTACK_WINDOW
    ;:_NET_REQUEST_FRAME_EXTENTS
    ))

(defparameter *application-window-properties*
  '(:_NET_WM_NAME
    ;:_NET_WM_VISIBLE_NAME
    ;:_NET_WM_ICON_NAME
    ;:_NET_WM_VISIBLE_ICON_NAME
    :_NET_WM_DESKTOP
    ))

(defparameter *netwm-supported*
  '(:_NET_SUPPORTING_WM_CHECK
    :_NET_NUMBER_OF_DESKTOPS
    :_NET_DESKTOP_GEOMETRY
    :_NET_DESKTOP_VIEWPORT
    :_NET_CURRENT_DESKTOP
    :_NET_WM_WINDOW_TYPE
    :_NET_WM_STATE
    :_NET_WM_STATE_MAXIMiZED_VERT
    :_NET_WM_STATE_MAXIMiZED_HORZ
    :_NET_WM_STATE_FULLSCREEN
    :_NET_WM_STATE_MODAL
    :_NET_WM_ALLOWED_ACTIONS
    :_NET_WM_STATE_FULLSCREEN
    :_NET_WM_STATE_HIDDEN
    :_NET_WM_STATE_DEMANDS_ATTENTION
    :_NET_WM_FULL_WINDOW_PLACEMENT
    :_NET_CLOSE_WINDOW
    :_NET_CLIENT_LIST
    :_NET_CLIENT_LIST_STACKING
    :_NET_ACTIVE_WINDOW
    :_NET_WM_DESKTOP
    :_NET_WORKAREA
    :_KDE_NET_SYSTEM_TRAY_WINDOW_FOR))

(defparameter *netwm-allowed-actions*
  '(:_NET_WM_ACTION_MOVE
    :_NET_WM_ACTION_RESIZE
    :_NET_WM_ACTION_MINIMIZE
    :_NET_WM_ACTION_SHADE
    :_NET_WM_ACTION_STICK
    :_NET_WM_ACTION_MAXIMIZE_HORZ
    :_NET_WM_ACTION_MAXIMIZE_VERT
    :_NET_WM_ACTION_FULLSCREEN
    :_NET_WM_ACTION_CHANGE_DESKTOP
    :_NET_WM_ACTION_CLOSE
    :_NET_WM_ACTION_ABOVE
    :_NET_WM_ACTION_BELOW))

(defparameter +net-wm-state-remove+ 0)
(defparameter +net-wm-state-add+ 1)
(defparameter +net-wm-state-toggle+ 2)

(defparameter +withdrawn-state+ 0)
(defparameter +normal-state+ 1)
(defparameter +iconic-state+ 3)

(defclass window-manager ()
  ((display :initarg :display :reader display)
   (screen :initarg :screen :reader screen)
   (root :initarg :root :reader root)
   (vdesks :initarg :vdesks :accessor vdesks)
   (current-vdesk :initarg :current-vdesk :accessor current-vdesk)
   (modifiers :accessor modifiers)
   (binds :initform '() :accessor binds)
   (supporting :accessor supporting)))

(defclass vdesk ()
  ((screen :initarg :screen :accessor vdesk-screen)
   (windows :initform '() :accessor vdesk-windows)
   (current-window :initform nil :accessor vdesk-current-window)))

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

(defun windows (*window-manager*)
  (vdesk-windows (current-vdesk *window-manager*)))

(defun (setf windows) (windows *window-manager*)
  (setf (vdesk-windows (current-vdesk *window-manager*)) windows))

(defun current-window (*window-manager*)
  (vdesk-current-window (current-vdesk *window-manager*)))

(defun (setf current-window) (window *window-manager*)
  (set-net-active-window (if window (window-xwin window) :none))
  (setf (vdesk-current-window (current-vdesk *window-manager*)) window))

(defun make-window-manager (display)
  (let* ((display (if display
                      (xlib:open-display "" :display display)
                      (xlib:open-default-display)))
         (screen (xlib:display-default-screen display))
         (root (xlib:screen-root screen))
         (vdesk (make-instance 'vdesk :screen screen)))
    (make-instance 'window-manager
                   :display display
                   :screen screen
                   :root root
                   :vdesks (list vdesk)
                   :current-vdesk vdesk)))

(defun wm-state (xwin)
  (first (xlib:get-property xwin :WM_STATE)))

(defun (setf wm-state) (state xwin)
  (xlib:change-property xwin :WM_STATE (list state) :WM_STATE 32))

(defun send-client-message (xwin type &rest data)
  (xlib:send-event xwin
                   :client-message nil
                   :window xwin
                   :type type
                   :format 32
                   :data data))

(defun get-wm-name (xwin)
  (or (alexandria:when-let (name (xlib:get-property xwin :_NET_WM_NAME))
        (babel:octets-to-string (coerce name '(vector (unsigned-byte 8)))))
      (values (xlib:wm-name xwin))))

(defun set-net-wm-allowed-actions (xwin)
  (xlib:change-property xwin :_NET_WM_ALLOWED_ACTIONS
                        (mapcar (lambda (a)
                                  (xlib:intern-atom (display *window-manager*) a))
                                *netwm-allowed-actions*)
                        :atom 32))

(defun set-net-current-desktop (index)
  (xlib:change-property (root *window-manager*)
                        :_NET_CURRENT_DESKTOP
                        (list index)
                        :cardinal 32))

(defun set-net-active-window (xwin)
  (xlib:change-property (root *window-manager*)
                        :_NET_ACTIVE_WINDOW
                        (list (if (eq xwin :none) xwin (xlib:drawable-id xwin)))
                        :window 32))

(defun set-net-client-list (windows)
  (xlib:change-property (root *window-manager*)
                        :_NET_CLIENT_LIST
                        (mapcar #'window-xwin windows)
                        :window 32
                        :transform #'xlib:drawable-id))

(defun set-net-client-list-stacking (windows)
  (xlib:change-property (root *window-manager*)
                        :_NET_CLIENT_LIST_STACKING
                        (mapcar #'window-xwin windows)
                        :window 32
                        :transform #'xlib:drawable-id))

(defun set-net-wm-desktop (window vdesk)
  (xlib:change-property (window-xwin window)
                        :_NET_WM_DESKTOP
                        (list (vdesk-index vdesk))
                        :cardinal 32))

(defun update-net-wm-desktop ()
  (loop :for index :from 0
        :for vdesk :in (vdesks *window-manager*)
        :do (dolist (window (vdesk-windows vdesk))
              (set-net-wm-desktop (window-xwin window) index))))

(defun update-net-number-of-desktops ()
  (xlib:change-property (root *window-manager*)
                        :_NET_NUMBER_OF_DESKTOPS
                        (list (length (vdesks *window-manager*)))
                        :cardinal 32))

(defun initialize-window-manager (*window-manager*)
  (init-modifiers)
  (grab-all)
  (setf (xlib:window-event-mask (root *window-manager*))
        '(:substructure-notify :substructure-redirect))
  (xlib:change-property (root *window-manager*)
                        :_NET_SUPPORTED
                        (mapcar (lambda (s)
                                  (xlib:intern-atom (display *window-manager*) s))
                                (remove-duplicates (append *root-window-properties*
                                                           *other-root-window-messages*
                                                           *netwm-supported*)))
                        :atom 32)
  (set-net-client-list (windows *window-manager*))
  (update-net-number-of-desktops)
  (xlib:change-property (root *window-manager*)
                        :_NET_DESKTOP_GEOMETRY
                        (list (xlib:screen-width (screen *window-manager*))
                              (xlib:screen-height (screen *window-manager*)))
                        :cardinal 32)
  (xlib:change-property (root *window-manager*)
                        :_NET_DESKTOP_VIEWPORT
                        (list 0 0)
                        :cardinal 32)
  (set-net-current-desktop 0)
  (xlib:change-property (root *window-manager*)
                        :_NET_WORKAREA
                        (list 0
                              0
                              (xlib:drawable-width (root *window-manager*))
                              (xlib:drawable-height (root *window-manager*)))
                        :cardinal 32)
  (let ((w (xlib:create-window :parent (root *window-manager*) :x 0 :y 0 :width 1 :height 1)))
    (setf (supporting *window-manager*) w)
    (xlib:change-property (root *window-manager*)
                          :_NET_SUPPORTING_WM_CHECK
                          (list w)
                          :window 32
                          :transform #'xlib:drawable-id)
    (xlib:change-property w :_NET_SUPPORTING_WM_CHECK
                          (list w)
                          :window 32
                          :transform #'xlib:drawable-id)
    (xlib:change-property w :_NET_WM_NAME
                          (string-downcase (package-name *package*))
                          :string 8
                          :transform #'xlib:char->card8))
  (dolist (xwin (xlib:query-tree (root *window-manager*)))
    (when (and (eq (xlib:window-override-redirect xwin) :off)
               (eq (xlib:window-map-state xwin) :viewable))
      (add-window xwin)))
  (alexandria:when-let (window (first (windows *window-manager*)))
    (focus-window window)))

(defun finalize-window-manager (*window-manager*)
  (xlib:destroy-window (supporting *window-manager*))
  (ungrab-all)
  (xlib:close-display (display *window-manager*)))

(defun event-loop ()
  (loop
   (xlib:process-event (display *window-manager*)
                       :handler #'handle-event
                       :discard-p t)))

(defun start-window-manager (*window-manager* &key initialized-hook)
  (initialize-window-manager *window-manager*)
  (when initialized-hook (funcall initialized-hook))
  (unwind-protect (event-loop)
    (finalize-window-manager *window-manager*)))

(defun run-program (command &key wait)
  (uiop:run-program (format nil
                            "DISPLAY=~A:~D; ~{~A~^ ~}~:[&~;~]"
                            (xlib:display-host (display *window-manager*))
                            (xlib:display-display (display *window-manager*))
                            (uiop:ensure-list command)
                            wait)))
