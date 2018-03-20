(defsystem "liwm"
  :depends-on ("clx" "alexandria")
  :serial t
  :components ((:file "package")
               (:file "logger")
               (:file "primitives")
               (:file "input")
               (:file "window")
               (:file "events")
               (:file "liwm")))
