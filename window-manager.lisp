(in-package :liwm)

(defvar *window-manager*)

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

(defclass window-manager ()
  ((display :initarg :display :reader display)
   (screen :initarg :screen :reader screen)
   (root :initarg :root :reader root)
   (windows :initform '() :accessor windows)
   (current-window :accessor current-window)
   (modifiers :accessor modifiers)
   (binds :initform '() :accessor binds)
   (supporting :accessor supporting)))

(defun make-window-manager (display)
  (let* ((display (if display
                      (xlib:open-display "" :display display)
                      (xlib:open-default-display)))
         (screen (xlib:display-default-screen display))
         (root (xlib:screen-root screen)))
    (make-instance 'window-manager :display display :screen screen :root root)))

(defun set-netwm-allowed-actions (xwin)
  (xlib:change-property xwin :_NET_WM_ALLOWED_ACTIONS
                        (mapcar (lambda (a)
                                  (xlib:intern-atom (display *window-manager*) a))
                                *netwm-allowed-actions*)
                        :atom 32))

(defun update-net-client-list ()
  (xlib:change-property (root *window-manager*)
                        :_NET_CLIENT_LIST
                        (mapcar #'window-xwin (windows *window-manager*))
                        :window 32
                        :transform #'xlib:drawable-id))

(defun initialize-window-manager (*window-manager*)
  (init-modifiers)
  (grab-all)
  (setf (xlib:window-event-mask (root *window-manager*))
        '(:substructure-notify :substructure-redirect))
  (bind-key (make-key-input "t" :meta t :control t)
            (lambda () (run-program "xterm" :wait nil)))
  (bind-key (make-key-input "n" :super t)
            (lambda () (focus-next-window)))
  (bind-key (make-key-input "p" :super t)
            (lambda () (focus-previous-window)))
  (bind-key (make-key-input "F4" :meta t)
            (lambda () (quit-window (current-window *window-manager*))))
  (bind-key (make-key-input "x" :super t)
            (lambda () (toggle-maximize-window (current-window *window-manager*))))
  (xlib:change-property (root *window-manager*)
                        :_NET_SUPPORTED
                        (mapcar (lambda (s)
                                  (xlib:intern-atom (display *window-manager*) s))
                                *netwm-supported*)
                        :atom 32)
  (xlib:change-property (root *window-manager*)
                        :_NET_CLIENT_LIST '() :window 32
                        :transform #'xlib:drawable-id)
  (let ((w (xlib:create-window :parent (root *window-manager*) :x 0 :y 0 :width 1 :height 1)))
    (setf (supporting *window-manager*) w)
    (xlib:change-property (root *window-manager*) :_NET_SUPPORTING_WM_CHECK
                          (list w) :window 32
                          :transform #'xlib:drawable-id)
    (xlib:change-property w :_NET_SUPPORTING_WM_CHECK
                          (list w) :window 32
                          :transform #'xlib:drawable-id))
  (xlib:change-property (root *window-manager*) :_NET_DESKTOP_GEOMETRY
                        (list (xlib:screen-width (screen *window-manager*))
                              (xlib:screen-height (screen *window-manager*)))
                        :cardinal 32)
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

(defun start-window-manager (*window-manager*)
  (initialize-window-manager *window-manager*)
  (unwind-protect (event-loop)
    (finalize-window-manager *window-manager*)))

(defun run-program (command &key wait)
  (uiop:run-program (format nil
                            "DISPLAY=~A:~D; ~{~A~^ ~}~:[&~;~]"
                            (xlib:display-host (display *window-manager*))
                            (xlib:display-display (display *window-manager*))
                            (uiop:ensure-list command)
                            wait)))
