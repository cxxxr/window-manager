(in-package :liwm)

(defun vdesk-index (vdesk)
  (position vdesk (vdesks *window-manager*)))

(defun current-vdesk-index ()
  (vdesk-index (current-vdesk *window-manager*)))

(defun get-vdesk (index)
  (nth index (vdesks *window-manager*)))

(defmethod (setf vdesks) :after (vdesks (*window-manager* window-manager))
  (log-format "(setf vdesks) vdesks = ~D" (length (vdesks *window-manager*)))
  (update-net-number-of-desktops)
  (update-net-wm-desktop))

(defmethod (setf current-vdesk) :after (vdesk (*window-manager* window-manager))
  (log-format "(setf current-vdesk) vdesk = ~A" vdesk)
  (set-net-current-desktop (vdesk-index vdesk))
  (focus-window (first (vdesk-windows vdesk))))

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

(defun change-to-nth-vdesk (n)
  (alexandria:when-let (vdesk (nth n (vdesks *window-manager*)))
    (change-to-vdesk vdesk)))

(defun find-vdesk-from-window (window)
  (dolist (vdesk (vdesks *window-manager*))
    (when (member window (vdesk-windows vdesk))
      (return vdesk))))

(defun move-window-to-vdesk (window vdesk)
  ;; TODO: modal window
  (let ((old-vdesk (find-vdesk-from-window window)))
    (unless (eq old-vdesk vdesk)
      (if (eq (current-vdesk *window-manager*) vdesk)
          (show-window window)
          (hide-window window))
      (alexandria:deletef (vdesk-windows old-vdesk) window)
      (push window (vdesk-windows vdesk))
      (set-net-wm-desktop window vdesk))))

(defun move-window-to-nth-vdesk (window n)
  (alexandria:when-let (vdesk (nth n (vdesks *window-manager*)))
    (move-window-to-vdesk window vdesk)))

(defun remove-vdesk-window (window)
  (let ((vdesk (find-vdesk-from-window window)))
    (alexandria:deletef (vdesk-windows vdesk) window)))
