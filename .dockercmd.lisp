(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(setf sb-impl::*default-external-format* :utf-8)
(setf sb-alien::*default-c-string-external-format* :utf-8)

(asdf:load-system :cl-lsystem :force t :verbose nil)

(let ((lsystem (sb-unix::posix-getenv "LSYS"))
      (cl-lsystem::*out* (sb-unix::posix-getenv "OUT")))
  (assert lsystem nil "You must provide an L-System definition file through the environment variable ENV. See the documentation for more information.")
  (assert cl-lsystem::*out* nil "You must provide an OUT environment variable specifying the path and the name of the file in which to write the rendenring of the process. See the documentation for more information.")
  (load lsystem))
