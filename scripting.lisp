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
                         (cl-arrows:->> (etypecase arg
                                          (symbol (symbol-name arg))
                                          (list (or (fourth arg)
                                                    (symbol-name (first arg)))))
                                        (substitute #\_ #\-)))
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
                            :turtle (make-instance 'turtle2d :direction direction))))
    (process lsystem n env)
    (save env out)))

(define-env-fun obj ((lsystem *lsystem*)
                     (n 0)
                     (out "out" identity)
                     (space (mat (1.0 0.0 0.0)
                                 (0.0 1.0 0.0)
                                 (0.0 0.0 1.0))
                            eval-read-from-string)
                     (branch-radius 1.0)
                     (branch-decay 1.0)
                     (edges-per-branch 16))
  (let ((env (make-instance 'obj-environment
                            :branch-radius branch-radius
                            :branch-decay branch-decay
                            :edges-per-branch edges-per-branch
                            :turtle (make-instance 'turtle3d :space space))))
    (process lsystem n env)
    (save env out)))

(defun define-3d-turtle (theta &optional (delta 3))
  "Defines the de-facto standard rules for a 3D turtle. `theta' represents how much to spin when a rotation is performed. `delta' represents how much units to move when a `jump' (and `forward') occurs. NOTE that the rules `/' and `\' for rolling are replaced by `>' and `<' because of the Common Lisp reader."
  (define-rule F () #i(forward delta))
  (define-rule G () #i(jump delta))
  (define-rule [ () #i(stack))
  (define-rule ] () #i(unstack))
  (define-rule { () #i(begin-fill))
  (define-rule } () #i(end-fill))
  (define-rule < () #i(roll theta))
  (define-rule > () #i(roll (- theta)))
  (define-rule ^ () #i(pitch theta))
  (define-rule & () #i(pitch (- theta)))
  (define-rule + () #i(yaw theta))
  (define-rule - () #i(yaw (- theta))))
