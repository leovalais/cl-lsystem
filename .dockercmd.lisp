(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(setf sb-impl::*default-external-format* :utf-8)
(setf sb-alien::*default-c-string-external-format* :utf-8)

(asdf:load-system :cl-lsystem/scripting :force t :verbose nil)
(in-package :cl-lsystem/scripting)

(let ((script (sb-unix::posix-getenv "SCRIPT")))
  (assert script nil "You must provide an L-System script file through the environment variable SCRIPT. See the documentation for more information.")
  (load script))
