(in-package :cl-lsystem)

(defclass instruction (standard-class)
  ((constructor :initform nil
                :initarg :constructor
                :reader constructor
                :documentation "The lambda list of the constructor to be found in the :turtle field of a L-system definition. E.g.: (defclass myinst ... (:constructor (a &key b))) matches (myinst 12 :b 13).")))

(defclass singleton-instruction (instruction)
  ((instance :initform nil)))

(defmethod sb-mop:validate-superclass ((class instruction) (superclass standard-class))
  t)

(defmethod make-instance ((class singleton-instruction) &key)
  (or (slot-value class 'instance)
      (let ((instance (call-next-method)))
        (setf (slot-value class 'instance) instance)
        instance)))

;; FIXME class not finalized and closer mop doesn't work
;; (defmethod initialize-instance :after ((class instruction) &key)
;;   (let* ((slots (sb-mop:class-slots class))
;;          (slot-names (mapcar #'sb-mop:slot-definition-name slots))
;;          (cons-lambda-list (second (constructor class))))
;;     (break)
;;     (when-let (diff (set-difference slot-names cons-lambda-list))
;;       (error "missing slots in constrcutor lambda list: ~{~S~^ ~}" diff))))

(defclass noop ()
  ()
  (:metaclass singleton-instruction))

(defclass jump ()
  (delta)
  (:metaclass instruction)
  (:constructor (delta)))

(defclass forward (jump)
  ((delta :type real))
  (:metaclass instruction)
  (:constructor (delta)))

(defclass rotate ()
  ((theta :type real))
  (:metaclass instruction)
  (:documentation "2D rotation")
  (:constructor (theta)))

(defclass roll ()
  ((theta :type real))
  (:metaclass instruction)
  (:documentation "3D X rotation")
  (:constructor (theta)))

(defclass pitch ()
  ((theta :type real))
  (:metaclass instruction)
  (:documentation "3D Y rotation")
  (:constructor (theta)))

(defclass yaw ()
  ((theta :type real))
  (:metaclass instruction)
  (:documentation "3D Z rotation")
  (:constructor (theta)))

(defclass stack ()
  ()
  (:metaclass singleton-instruction))

(defclass unstack ()
  ()
  (:metaclass singleton-instruction))

(defclass begin-fill ()
  ()
  (:metaclass singleton-instruction)
  (:documentation "Starts recording vertices on top of a new stacked polygon (cf. `3d-environment/fill-stack'."))

(defclass end-fill ()
  ()
  (:metaclass singleton-instruction)
  (:documentation "Pops the polygon on top of `3d-environment/fill-stack' and fills it."))

(defclass lisp ()
  ((procedure :type cl:function))
  (:metaclass instruction)
  (:documentation "Applies the given function. Its argument is the environment.")
  (:constructor (procedure)))

(defun eval-instruction-constructor (sexp)
  (destructuring-bind (class-name &rest args) sexp
    (let* ((class (find-class class-name))
           (lambda-list (car (constructor class))))
      (closer-mop:ensure-finalized class)
      (let* ((slots (sb-mop:class-slots class))
             (slot-names (mapcar #'sb-mop:slot-definition-name slots)))
        (cl:eval
         (with-gensyms (instruction)
           `(let ((,instruction (make-instance ',class-name)))
              (destructuring-bind ,lambda-list (list ,@args)
                ,@(mapcar (lambda (name)
                            `(setf (slot-value ,instruction ',name) ,name))
                          slot-names))
              ,instruction)))))))
