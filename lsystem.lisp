(in-package :cl-lsystem)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Definitions

(deftype parametrized-letter ()
  '(or symbol (rte:rte (:cat symbol (:* t)))))

(deftype parametrized-word ()
  '(rte:rte (:* parametrized-letter)))

(defclass rule ()
  ((name :initform (error "a rule must have a name")
         :initarg :name
         :reader name
         :type symbol)
   (function :initform nil
             :initarg :function
             :accessor function
             :type (or null cl:function))
   (instruction :initform (error "a rule must have an instruction")
                :initarg :instruction
                :accessor instruction
                :type cl:function)))

(defclass lsystem ()
  ((axiom :initform ()
          :initarg :axiom
          :reader axiom
          :type parametrized-word)
   (rules :initform (make-hash-table)
          :initarg :rules
          :accessor rules
          :type hash-table)))

(defgeneric set-rule (lsystem rule)
  (:method ((lsystem lsystem) (rule rule))
    (with-slots (name) rule
      (when (gethash name (rules lsystem))
        (warn "redefining rule ~a for lsystem ~S" name lsystem))
      (setf (gethash name (rules lsystem))
            rule))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Axiom expansion

(defun rule-of-parametrized-letter (lsystem letter)
  (declare (type lsystem lsystem)
           (type parametrized-letter letter))
  (etypecase letter
    (symbol
     (values (gethash letter (rules lsystem))
             nil))
    (list
     (destructuring-bind (name &rest args)
         letter
       (values (gethash name (rules lsystem))
               args)))))

(defun parametrized-letter->instruction (lsystem letter)
  (declare (type lsystem lsystem)
           (type parametrized-letter letter))
  (multiple-value-bind (rule args)
      (rule-of-parametrized-letter lsystem letter)
    (assert rule nil "rule not found in L-System ~S for parametrized letter ~S" lsystem letter)
    (apply (instruction rule)
           args)))

(defun expand-letter (lsystem letter)
  (declare (type lsystem lsystem)
           (type parametrized-letter letter))
  (multiple-value-bind (rule args)
      (rule-of-parametrized-letter lsystem letter)
    (if rule
        (if-let (f (function rule))
          (apply (function rule)
                 args)
          (list letter))
        (list letter))))

(defun %iter-lsystem (lsystem n procedure word)
  (declare (type lsystem lsystem)
           (type unsigned-byte n)
           (type cl:function procedure))

  ;; last iteration?
  (if (zerop n)
      ;; call the procedure for each letter of the final word section
      (dolist (letter word)
        (let ((instruction (parametrized-letter->instruction lsystem letter)))
          (funcall procedure instruction)))

      ;; otherwise expand and recurse
      (dolist (letter word)
        (let ((new-word (expand-letter lsystem letter)))
          (%iter-lsystem lsystem (1- n) procedure new-word))))
  (values))

(defun iter-lsystem (lsystem n procedure)
  (declare (type lsystem lsystem)
           (type unsigned-byte n)
           (type cl:function procedure))
  (%iter-lsystem lsystem n procedure (axiom lsystem))
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; define-lsystem, define-rule, #w and #i

(defparameter *lsystem* nil)

(defmacro define-lsystem (name axiom)
  `(defparameter ,name (setf *lsystem*
                             (make-instance 'lsystem :axiom ,axiom))))

(defmacro define-rule (name lambda-list instruction &body body)
  (let* ((ll (set-difference lambda-list lambda-list-keywords))
         (ignore-statement (etypecase ll
                             ((rte:rte (:+ symbol)) `((declare (ignorable ,@ll))))
                             (list                  '()))))
    `(set-rule *lsystem*
               (make-instance 'rule
                              :name ',name
                              :function ,(if body
                                             `(lambda ,lambda-list
                                                ,@ignore-statement
                                                ,@body)
                                             nil)
                              :instruction (lambda ,lambda-list
                                             ,@ignore-statement
                                             ,instruction)))))

(defmacro parametric-word (&rest words)
  (flet ((word->list (word)
             (destructuring-bind (rule &rest parameters)
                 word
               `(list ',rule ,@parameters))))
    (if (= 1 (length words)) ; (FAF) => (list (list 'F) (list 'A) (list 'F))
        `(list ,@(map 'list
                      (lambda (character)
                        ',(symbolicate character))
                      (symbol-name (first words))))
        (etypecase (first words)
          (symbol ; (A 1 2) => (list (list 'A 1 2))
           `(list ,(word->list words)))
          ((and list
                (not null)) ; ((A) (B 1)) => (list (list 'A) (list 'B 1))
           (cons 'list (mapcar #'word->list words)))))))

(defmacro instruction-constructor ((class-name &rest args))
  (let* ((class (find-class class-name))
         (lambda-list (car (constructor class))))
    (closer-mop:ensure-finalized class)
    (let* ((slots (sb-mop:class-slots class))
           (slot-names (mapcar #'sb-mop:slot-definition-name slots)))
      (with-gensyms (instruction)
        `(let ((,instruction (make-instance ',class-name)))
           (destructuring-bind ,lambda-list (list ,@args)
             ,@(mapcar (lambda (name)
                         `(setf (slot-value ,instruction ',name) ,name))
                       slot-names))
           ,instruction)))))

(set-dispatch-macro-character #\# #\w
                              (lambda (stream subchar arg)
                                (declare (ignore subchar arg))
                                (let ((words (read stream t nil t)))
                                  (etypecase words
                                    (symbol `(parametric-word ,words))
                                    (list `(parametric-word ,@words))))))

(set-dispatch-macro-character #\# #\i
                              (lambda (stream subchar arg)
                                (declare (ignore subchar arg))
                                (let ((instruction (read stream t nil t)))
                                  `(instruction-constructor ,instruction))))
