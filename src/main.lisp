(in-package :window-manager)

(defun error-handler (display error-key &rest key-vals &key asynchronous &allow-other-keys)
  (cond
    ((and asynchronous
          (find error-key '(xlib:window-error xlib:drawable-error xlib:match-error)))
     (log-format "Ignoring error: ~s~%" error-key))
    ((eq error-key 'xlib:access-error)
     (write-line "Another window manager is running."))
    (asynchronous
     (format t "Caught Asynchronous X Error: ~s ~s" error-key key-vals))
    (t
     (apply 'error error-key :display display :error-key error-key key-vals))))

(defun main (&key display)
  (start-window-manager
   (make-window-manager display)
   :initialized-hook (lambda ()
                       (setf (xlib:display-error-handler (display *window-manager*)) 'error-handler)
                       #+lispworks (setf *wm-thread* mp:*current-process*)
                       (add-vdesk)
                       (add-vdesk)
                       (add-vdesk)
                       (bind-key (make-key-input "t" :meta t :control t)
                                 (get-command "xterm"))
                       (bind-key (make-key-input "n" :super t)
                                 (get-command "next window"))
                       (bind-key (make-key-input "p" :super t)
                                 (get-command "previous window"))
                       (bind-key (make-key-input "F4" :meta t)
                                 (get-command "quit window"))
                       (bind-key (make-key-input "x" :super t)
                                 (get-command "maximize window"))
                       (bind-key (make-key-input "F9" :meta t)
                                 (get-command "minimize window"))

                       (bind-key (make-key-input "1" :super t)
                                 (get-command "change desktop 1"))
                       (bind-key (make-key-input "2" :super t)
                                 (get-command "change desktop 2"))
                       (bind-key (make-key-input "3" :super t)
                                 (get-command "change desktop 3"))
                       (bind-key (make-key-input "4" :super t)
                                 (get-command "change desktop 4"))

                       (bind-key (make-key-input "1" :super t :control t)
                                 (get-command "move window to desktop 1"))
                       (bind-key (make-key-input "2" :super t :control t)
                                 (get-command "move window to desktop 2"))
                       (bind-key (make-key-input "3" :super t :control t)
                                 (get-command "move window to desktop 3"))
                       (bind-key (make-key-input "4" :super t :control t)
                                 (get-command "move window to desktop 4"))

                       #+lispworks
                       (bind-key (make-key-input "F12" :super t)
                                 (lambda ()
                                   (capi:display (make-instance 'lw-tools:listener))))
                       #+lispworks
                       (bind-key (make-key-input "F11" :super t)
                                 (lambda ()
                                   (capi:display (make-instance 'lw-tools:editor)))))))
