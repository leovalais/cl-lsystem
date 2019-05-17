(in-package :cl-lsystem)

(defun list->vector (l)
  (map 'vector #'identity l))

(defun parse-rule (sexp-rule)
  (destructuring-bind (left right) sexp-rule
    (make-instance 'rule
                   :left left
                   :right (list->vector right))))

(defun parse-grammar (sexp)
  (make-instance 'grammar
                 :axiom (list->vector (getf sexp :axiom))
                 :rules (map 'vector #'parse-rule (getf sexp :rules))))

(defun parse-mapping (sexp-map)
  (let ((mapping (make-hash-table)))
    (iter (for (letter constructor) in sexp-map)
      (let ((instruction (eval-instruction-constructor constructor)))
        (setf (gethash letter mapping) instruction)))
    mapping))

(defun parse (sexp)
  (let ((lsystem (make-instance 'lsystem
                                :grammar (parse-grammar sexp)
                                :mapping (parse-mapping (getf sexp :turtle)))))
    (if-let (d (cl:eval (getf sexp :initial-direction)))
      (setf (initial-direction lsystem)
            d))
    lsystem))
