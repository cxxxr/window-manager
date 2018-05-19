(in-package :window-manager)

(defun next-element (elt list &key (test #'eql))
  (or (second (member elt list :test test))
      (first list)))

(defun previous-element (elt list &key (test #'eql))
  (if (eq elt (first list))
      (alexandria:lastcar list)
      (loop :for rest :on list
            :do (when (funcall test elt (second rest))
                  (return (first rest))))))
