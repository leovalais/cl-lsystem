(in-package :cl-lsystem/scripting)

(defmacro define-env-fun (name (&rest arguments) &body body)
  (let* ((vars (mapcar (lambda (arg)
                         (etypecase arg
                           (symbol arg)
                           (list (first arg))))
                       arguments))
         (defs (mapcar (lambda (arg)
                         (etypecase arg
                           (symbol nil)
                           (list (or (second arg)
                                     nil))))
                       arguments))
         (evls (mapcar (lambda (arg)
                         (etypecase arg
                           (symbol 'read-from-string)
                           (list (or (third arg)
                                     'read-from-string))))
                       arguments))
         (envs (mapcar (lambda (arg)
                         (etypecase arg
                           (symbol (symbol-name arg))
                           (list (or (fourth arg)
                                     (symbol-name (first arg))))))
                       arguments)))
    `(defun ,name (&key ,@(mapcar (lambda (var def)
                                    (list var def))
                                  vars defs))
       ,@(mapcar (lambda (var env evaluator)
                   (let ((varenv (gensym (concatenate 'string "ENV-" (symbol-name var)))))
                     `(let ((,varenv (sb-unix::posix-getenv ,env)))
                        (when ,varenv
                          (setf ,var
                                ,(if (symbolp evaluator)
                                     `(funcall #',evaluator ,varenv)
                                     `(funcall ,evaluator ,varenv)))))))
                 vars envs evls)
       ,@body)))

(defun eval-read-from-string (&rest args)
  (eval (apply #'read-from-string args)))

(define-env-fun png ((lsystem *lsystem*)
                     (n 0)
                     (out "out" identity)
                     (origin (v 0.0 0.0) eval-read-from-string)
                     (width 500)
                     (height 500)
                     (direction (v 1.0 0.0)))
  (let ((env (make-instance 'png-environment
                            :origin origin
                            :width width
                            :height height
                            :turtle (make-instance 'turtle2D
                                                   :direction direction))))
    (process lsystem n env)
    (save env out)))
