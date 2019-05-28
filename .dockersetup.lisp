(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(setf sb-impl::*default-external-format* :utf-8)
(setf sb-alien::*default-c-string-external-format* :utf-8)

(dolist (asd (rest *posix-argv*))
  (with-open-file (in asd)
    (let* ((system (cddr (read in)))
           (deps (getf system :depends-on)))
      (setf deps (remove :array-operations deps))
      (mapc #'ql:quickload deps))))
