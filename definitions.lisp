(in-package :cl-lsystem)

(defmacro required (slot)
  `(error "required field: ~S" ',slot))

(deftype letter ()
  'symbol)

(deftype word ()
  '(array letter 1))

(defclass rule ()
  ((left :initform (required left)
         :initarg :left
         :reader left
         :type letter)
   (right :initform (required right)
          :initarg :right
          :reader right
          :type word)
   (proba :initform 1.0
          :initarg :proba
          :type (float (0.0) 1.0))))

(defclass grammar ()
  ((axiom :initform (required axiom)
          :initarg :axiom
          :accessor axiom
          :type word)
   (rules :initform (required rules)
          :initarg :rules
          :accessor rules
          :type (array rule 1))))

(defclass lsystem ()
  ((grammar :initform (required grammar)
            :initarg :grammar
            :accessor grammar
            :type grammar)
   (mapping :initform (required mapping)
            :initarg :mapping
            :accessor mapping
            :type letter-mapping)
   (initial-orientation :initform nil
                        :initarg :initial-orientation
                        :accessor initial-orientation
                        :type (or null
                                  real
                                  V3))))

(deftype letter-mapping ()
  'hash-table)
  ;; '(rte:rte (:+ (rte:rte (:cat letter turtle-move)))))
