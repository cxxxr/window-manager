(in-package :liwm)

(defclass vdesk ()
  ((screen :initarg :screen :accessor vdesk-screen)
   (windows :initform '() :accessor vdesk-windows)
   (current-window :initform nil :accessor vdesk-current-window)))
