(in-package :liwm)

(defun vdesk-index (vdesk)
  (position vdesk (vdesks *window-manager*)))

(defmethod (setf vdesks) :after (vdesks (*window-manager* window-manager))
  (log-format "(setf vdesks) vdesks = ~D" (length (vdesks *window-manager*)))
  (update-net-number-of-desktops))

(defmethod (setf current-vdesk) :after (vdesk (*window-manager* window-manager))
  (log-format "(setf current-vdesk) vdesk = ~A" vdesk)
  (set-net-current-desktop (vdesk-index vdesk)))

(defun get-next-vdesk (vdesk)
  (next-element vdesk (vdesks *window-manager*)))

(defun get-previous-vdesk (vdesk)
  (previous-element vdesk (vdesks *window-manager*)))

(defun add-vdesk ()
  (let ((vdesk (make-instance 'vdesk :screen (screen *window-manager*))))
    (alexandria:nconcf (vdesks *window-manager*)
                       (list vdesk))))

(defun delete-vdesk (vdesk)
  (when (< 1 (length (vdesks *window-manager*)))
    (when (eq vdesk (current-vdesk *window-manager*))
      (change-to-vdesk (get-next-vdesk vdesk)))
    (alexandria:deletef (vdesks *window-manager*) vdesk)))

(defun change-to-vdesk (new-vdesk)
  (let ((old-vdesk (current-vdesk *window-manager*)))
    (unless (eq new-vdesk old-vdesk)
      (mapc #'show-window (reverse (vdesk-windows new-vdesk)))
      (mapc #'hide-window (vdesk-windows old-vdesk))
      (setf (current-vdesk *window-manager*) new-vdesk))))
