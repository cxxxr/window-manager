(in-package :liwm)

(defvar *display*)
(defvar *screen*)
(defvar *root*)

(defvar *last-mouse-x* 0)
(defvar *last-mouse-y* 0)
(defvar *last-mouse-state* nil)

(defun configure-window (xwin &key x y width height)
  (when x
    (setf (xlib:drawable-x xwin) x))
  (when y
    (setf (xlib:drawable-y xwin) y))
  (when width
    (setf (xlib:drawable-width xwin) width))
  (when height
    (setf (xlib:drawable-height xwin) height)))
