(in-package :liwm)

(defun main (&key display)
  (start-window-manager (make-window-manager display)))
