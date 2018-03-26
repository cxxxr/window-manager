(in-package :liwm)

(defun event-loop ()
  (loop
   (xlib:process-event (display *window-manager*) :handler #'handle-event :discard-p t)))

(defun main (&key display)
  (let ((*window-manager* (make-window-manager display)))
    (initialize-window-manager)
    (bind-key (make-key-input "t" :meta t :control t)
              (lambda () (run-program "xterm" :wait nil)))
    (dolist (xwin (xlib:query-tree (root *window-manager*)))
      (when (and (eq (xlib:window-override-redirect xwin) :off)
                 (eq (xlib:window-map-state xwin) :viewable))
        (add-window xwin)))
    (unwind-protect (event-loop)
      (finalize-window-manager))))
