(in-package :deskbar)

(defun current-time-string ()
  (multiple-value-bind (second minute hour)
      (decode-universal-time (get-universal-time))
    (format nil "~2,'0D:~2,'0D:~2,'0D" hour minute second)))

