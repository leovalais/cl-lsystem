(in-package :cl-lsystem)

(defun apply-initial-orientation (lsystem env)
  (if-let (io (initial-orientation lsystem))
    (let ((turn (make-instance 'turn)))
      (setf (slot-value turn 'angle)
            io)
      (eval turn env))))

(defun process (sexp n &optional
                         (env (make-instance 'png-environment))
                         (filename "out"))
  (let* ((lsystem (parse sexp))
         (word (expand (grammar lsystem) n)))
    (apply-initial-orientation lsystem env)
    (iter (for letter in-vector word)
      (let ((instruction (gethash letter (mapping lsystem))))
        (assert instruction nil "no instruction for ~a" letter)
        (eval instruction env)))
    (save env filename)
    :ok))
