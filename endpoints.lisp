(in-package :cl-lsystem)

(defparameter *out* "out")

(defun process (lsystem n &key
                            (env (make-instance 'obj-environment))
                            (out *out*))
  (iter-lsystem lsystem n
                (lambda (instruction)
                  (eval instruction env)))
  (save env out)
  :ok)
