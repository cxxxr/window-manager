(in-package :liwm)

(defun vdesk-index (vdesk)
  (position vdesk (vdesks *window-manager*)))

(defun add-vdesk ()
  (let ((vdesk (make-instance 'vdesk :screen (screen *window-manager*))))
    (alexandria:nconcf (vdesks *window-manager*)
                       (list vdesk))
    (update-net-number-of-desktops)))

(defun change-to-vdesk (new-vdesk)
  (let ((old-vdesk (current-vdesk *window-manager*)))
    (unless (eq new-vdesk old-vdesk)
      (mapc #'show-window (reverse (vdesk-windows new-vdesk)))
      (mapc #'hide-window (vdesk-windows old-vdesk))
      (setf (current-vdesk *window-manager*) new-vdesk)
      (set-net-current-desktop (vdesk-index new-vdesk)))))
