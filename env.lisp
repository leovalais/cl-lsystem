(in-package :cl-lsystem)

;; NOTE `direction' should always be a unit vector
(defclass turtle2D ()
  ((position :initform (v 0 0)
             :initarg :position
             :accessor position
             :type V2)
   (direction :initform (v 1 0)
             :initarg :direction
             :accessor direction
             :type V2)))

;; NOTE `direction' should always be a unit vector
(defclass turtle3D ()
  ((position :initform (v 0 0 0)
             :initarg :position
             :accessor position
             :type V3)
   (direction :initform (v 1 0 0)
              :initarg :direction
              :accessor direction
              :type V3)))

(defclass environment ()
  ((stack :initform ()
          :accessor env-stack
          :type list)))

(defclass 2d-environment (environment)
  ((turtle :initform (make-instance 'turtle2D)
           :type turtle2D
           :accessor turtle)))

(defclass 3d-environment (environment)
  ((turtle :initform (make-instance 'turtle3D)
           :type turtle3D
           :accessor turtle)))

;; env initialization in a initialize-instance :after method

(defgeneric save (env filename))
(defgeneric stack (env)
  (:method-combination progn :most-specific-last))
(defgeneric unstack (env)
  (:method-combination progn :most-specific-first))

(defgeneric update-turtle (env &key position direction)
  (:method ((env environment) &key position direction)
    (let ((turtle (turtle env)))
      (when position
        (setf (position turtle) position))
      (when direction
        (setf (direction turtle) direction)))))

(defmethod stack progn ((env environment))
  (with-slots (turtle) env
    (push turtle (env-stack env))
    (setf turtle (make-instance (class-of turtle)
                                :position (position turtle)
                                :direction (direction turtle)))))

(defmethod unstack progn ((env environment))
  (setf (turtle env) (pop (env-stack env))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; PNG using VECTO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass png-environment (2d-environment)
  ((vecto-graphics-state :accessor vecto-graphics-state)
   (width :initform 2000
          :initarg :width)
   (height :initform 2000
           :initarg :height)
   (origin :initform (v 0 0)
           :initarg :origin
           :type V2)))


(defmethod initialize-instance :after ((env png-environment) &key)
  ;; setup vecto's graphic context
  (setf (vecto-graphics-state env)
        (make-instance 'vecto::graphics-state))

  ;; setup the context (brush & origin position)
  (with-slots (width height origin) env
    (vecto::state-image (vecto-graphics-state env) width height)
    (let* ((translation (v+ (v (/ width 2)
                               (/ height 2))
                            origin))
           (-translation (v- translation)))
      (eval-in-graphics-state env (lambda ()
                                    (vecto:set-rgb-fill 1.0 1.0 1.0)
                                    (vecto:set-rgb-stroke 0.0 0.0 0.0)
                                    (vecto:set-line-width 2)
                                    (vecto:translate (vx translation) (vy translation))
                                    (vecto:rectangle (vx -translation) (vy -translation)
                                                     width height)
                                    (vecto:move-to 0 0))))))

(defgeneric eval-in-graphics-state (env fun)
  (:method ((env png-environment) fun)
    (let ((vecto::*graphics-state* (vecto-graphics-state env)))
      (funcall fun))))

(defmethod save ((env png-environment) filename)
  (let ((vecto::*graphics-state* (vecto-graphics-state env)))
    (vecto:fill-and-stroke)
    (vecto:save-png (format nil "~a.png" filename))
    (vecto::clear-state vecto::*graphics-state*)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; VRML (.wrl)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass vrml-environment (3d-environment)
  ((shapes :initform (make-hash-table) ; shape names are symbols, so eql is ok
           :initarg :shapes
           :accessor shapes
           :type hash-table) ; NAME * vrml-shape TODO implement that
   (transform :initform (make-instance 'vrml-translation)
              :initarg :transform
              :accessor transform
              :type vrml-transformation)))

(defmethod initialize-instance :after ((env vrml-environment) &key)
  (with-slots (shapes transform) env
    (setf (gethash nil shapes) transform)))

(defclass vrml-shape ()
  ((material :initform nil
             :type null ; FIXME
             :accessor material
             :initarg :material)
   (geometry :initform (required geometry)
             :type vrml-geometry
             :accessor geometry
             :initarg :geometry)))

(defclass vrml-geometry () ())

(defclass vrml-cylinder (vrml-geometry)
  ((radius :initform 1
           :initarg :radius
           :accessor radius
           :type real)
   (height :initform 1
           :initarg :height
           :accessor height
           :type real)))

(defclass vrml-transformation (vrml-geometry)
  ((children :initform ()
             :initarg :children
             :accessor children
             :type list)))
(defclass vrml-translation (vrml-transformation)
  ((vect :initform (v 0 0 0)
         :initarg :vect
         :accessor vect
         :type vect)))
(defclass vrml-rotation (vrml-transformation)
  ((vect :initform (v 0 0 0)
         :initarg :vect
         :accessor vect
         :type vect)
   (spin :initform 1.0
         :initarg :spin
         :accessor spin
         :type real)))


(defmethod stack progn ((env vrml-environment))
  (with-slots (transform stack) env
    (push transform stack)))

(defmethod unstack progn ((env vrml-environment))
  (setf (transform env) (pop (env-stack env))))

(defmethod save ((env vrml-environment) filename)
  (with-open-file (wrl (format nil "~a.wrl" filename)
                       :direction :output
                       :if-exists :supersede)
    (format wrl "~a" (export-vrml env)))
  (values))


(defgeneric export-vrml (vrml)
  (:documentation "VRML2string"))

(defmethod export-vrml ((env vrml-environment))
  (flet ((maphash->list (fun ht)
           (let ((l ()))
             (maphash (lambda (k v)
                        (push (funcall fun k v) l))
                      ht)
             l)))
    (format nil "#VRML V2.0 utf8~%~{~%~a~}"
            (maphash->list (lambda (k v)
                             (declare (ignore k))
                             (export-vrml v))
                           (shapes env)))))
;; (format nil "#VRML V2.0 utf8~%~%~{~a~^~&~%~}"
;;         (mapcar #'export-vrml (shapes env))))

(defmethod export-vrml ((shape vrml-shape))
  (format nil "~&Shape {~%~a~&~a~&}"
          ""; (export-vrml (material shape))
          (export-vrml (geometry shape))))

(defmethod export-vrml ((cylinder vrml-cylinder))
  (format nil "~&geometry Cylinder {~%radius ~f~%height ~f~%}"
          (radius cylinder)
          (height cylinder)))

(defmethod export-vrml ((translation vrml-translation))
  (with-slots (vect children) translation
    (format nil "~&Transform {~%translation ~f ~f ~f~%children [~{~&~a~}~&]~%}"
            (vx vect) (vy vect) (vz vect)
            (mapcar #'export-vrml children))))

(defmethod export-vrml ((translation vrml-rotation))
  (with-slots (vect spin children) translation
    (format nil "~&Transform {~%rotation ~f ~f ~f ~f~%children [~{~&~a~}~&]~%}"
            (vx vect) (vy vect) (vz vect) spin
            (mapcar #'export-vrml children))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Wavefront OBJ
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sxhash-vect (v)
  (sxhash (map 'list #'identity v)))
(sb-ext:define-hash-table-test v= sxhash-vect)

(defclass obj-environment (environment)
  ((vertices :initform (make-hash-table :test 'v=)
             :accessor vertices)
   (lines :initform ()
          :accessor lines)
   (faces :initform ()
          :accessor faces)
   (current-index :initform 1)))

(defmethod initialize-instance :after ((env obj-environment) &key)
  (with-slots (vertices current-index) env
    (setf (gethash (v 0 0 0) vertices)
          current-index)
    (incf current-index)))

(defmethod save ((env obj-environment) filename)
  (with-open-file (obj (format nil "~a.obj" filename)
                       :direction :output
                       :if-exists :supersede)
    (with-slots (vertices lines faces) env
      (labels ((dump-v (v)
                 (format obj "~&v ~f ~f ~f" (vx v) (vy v) (vz v)))
               (index-of (v)
                 (gethash v vertices))
               (dump-l (l)
                 (destructuring-bind (src . dst) l
                   (format obj "~&l ~a ~a" (index-of src) (index-of dst))))
               (dump-f (f)
                 (format obj "~&f~{ ~a~}" (mapcar #'index-of f))))
        (maphash-keys #'dump-v vertices)
        (dump-v (v 0 0 0))
        (mapc #'dump-f faces)
        (mapc #'dump-l lines))))
  (values))
