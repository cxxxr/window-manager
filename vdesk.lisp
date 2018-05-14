(in-package :liwm)

(defun vdesk-index (vdesk)
  (position vdesk (vdesks *window-manager*)))

(defmethod (setf vdesks) :after (vdesks (*window-manager* window-manager))
  (log-format "(setf vdesks) vdesks = ~D" (length (vdesks *window-manager*)))
  (update-net-number-of-desktops))

(defmethod (setf current-vdesk) :after (vdesk (*window-manager* window-manager))
  (log-format "(setf current-vdesk) vdesk = ~A" vdesk)
  (set-net-current-desktop (vdesk-index vdesk)))

(defun add-vdesk ()
  (let ((vdesk (make-instance 'vdesk :screen (screen *window-manager*))))
    (alexandria:nconcf (vdesks *window-manager*)
                       (list vdesk))))

(defun change-to-vdesk (new-vdesk)
  (let ((old-vdesk (current-vdesk *window-manager*)))
    (unless (eq new-vdesk old-vdesk)
      (mapc #'show-window (reverse (vdesk-windows new-vdesk)))
      (mapc #'hide-window (vdesk-windows old-vdesk))
      (setf (current-vdesk *window-manager*) new-vdesk))))
