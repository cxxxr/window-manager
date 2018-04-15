(in-package :liwm)

(defparameter *netwm-symbols*
  '(:_NET_SUPPORTING_WM_CHECK
    :_NET_NUMBER_OF_DESKTOPS
    :_NET_DESKTOP_GEOMETRY
    :_NET_DESKTOP_VIEWPORT
    :_NET_CURRENT_DESKTOP
    :_NET_WM_WINDOW_TYPE
    :_NET_WM_STATE
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

(defun initialize-netwm ()
  (xlib:change-property (root *window-manager*)
                        :_NET_SUPPORTED
                        (mapcar (lambda (s)
                                  (xlib:intern-atom (display *window-manager*) s))
                                *netwm-symbols*)
                        :atom 32))

(defmethod initialize-window-manager ((*window-manager* window-manager))
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
  (initialize-netwm)
  (dolist (xwin (xlib:query-tree (root *window-manager*)))
    (when (and (eq (xlib:window-override-redirect xwin) :off)
               (eq (xlib:window-map-state xwin) :viewable))
      (add-window xwin)))
  (alexandria:when-let (window (first (windows *window-manager*)))
    (focus-window window)))

(defmethod finalize-window-manager ((*window-manager* window-manager))
  (ungrab-all)
  (xlib:close-display (display *window-manager*)))

(defun main (&key display)
  (run-window-manager (make-window-manager display)))
