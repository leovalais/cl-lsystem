(in-package :cl-lsystem)

(defun find-rule (grammar letter)
  (find letter (rules grammar)
        :key #'left))

(defun vector-append-extend (new-vector vector)
  (loop :for x :across new-vector
        :do (vector-push-extend x vector))
  (values))

(defun expand (grammar n)
  (if (zerop n)
      (axiom grammar)
      (let* ((word (expand grammar (1- n)))
             (expanded (make-array 0 :element-type 'letter :adjustable t :fill-pointer t)))
        (iter (for letter in-vector word)
          (if-let (rule (find-rule grammar letter))
            (vector-append-extend (right rule) expanded)
            (vector-push-extend letter expanded)))
        expanded)))
