(in-package :window-manager)

(defvar *logger-stream* *error-output*)

(defun log-format (string &rest args)
  (apply #'format *logger-stream* string args)
  (terpri *logger-stream*))
