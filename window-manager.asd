(defsystem "window-manager"
  :depends-on ("clx"
               "cl-xkeysym"
               "alexandria"
               "babel")
  :serial t
  :components ((:file "src/package")
               (:file "src/util")
               (:file "src/logger")
               (:file "src/window-manager")
               (:file "src/vdesk")
               (:file "src/input")
               (:file "src/window")
               (:file "src/events")
               (:file "src/command")
               (:file "src/main")))
