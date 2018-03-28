(in-package :liwm)

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
  (dolist (xwin (xlib:query-tree (root *window-manager*)))
    (when (and (eq (xlib:window-override-redirect xwin) :off)
               (eq (xlib:window-map-state xwin) :viewable))
      (add-window xwin)))
  (focus-window (first (windows *window-manager*))))

(defmethod finalize-window-manager ((*window-manager* window-manager))
  (ungrab-all)
  (xlib:close-display (display *window-manager*)))

(defun main (&key display)
  (run-window-manager display))
