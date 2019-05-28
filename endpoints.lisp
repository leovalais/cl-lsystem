(in-package :cl-lsystem)

(defun process (lsystem n &optional (env (make-instance 'obj-environment)))
  (iter-lsystem lsystem n
                (lambda (instruction)
                  (eval instruction env))))
