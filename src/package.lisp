(defpackage :window-manager
  (:nicknames :wm)
  (:use :cl)
  (:export
   :main))

#+lispworks
(defpackage :window-manager.api
  (:add-use-defaults t)
  (:export
   :get-window-list
   :get-window-name
   :focus-window))
