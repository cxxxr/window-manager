(in-package :cl-user)

(load-all-patches)

(lw:set-default-character-element-type 'cl:character)

(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(uiop:symbol-call :ql :quickload :window-manager)

(push '("wm" (:priority 60000000 :restart-action :continue) window-manager:main)
      mp:*initial-processes*)

(save-image "wm"
            :console t
            :multiprocessing t
            :environment nil)
