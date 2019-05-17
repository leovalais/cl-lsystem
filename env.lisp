(in-package :cl-lsystem)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Turtles

(defclass turtle ()
  ((position :initform nil
             :initarg :position
             :accessor position
             :type vect)))

;; NOTE `direction' should always be a unit vector
(defclass turtle2d (turtle)
  ((direction :initform (v 1.0 0.0)
              :initarg :direction
              :accessor direction
              :type V2)))

(defclass turtle3d (turtle)
  ((space :initform (mat (1.0 0.0 0.0)
                         (0.0 1.0 0.0)
                         (0.0 0.0 1.0))
          :initarg :space
          :accessor space
          :type (matrix 3 3)))
  (:documentation
   "A 3D turtle is a position and a unit matrix [H L U] `direction' which form a base of the 3D space.
- H is the direction of the `head' of the turtle.
- L is the direction of the `left' of the turtle.
- U is the direction above the turtle (see `up')."))


(defmethod initialize-instance :after ((turtle turtle2d) &key)
  (with-slots (position) turtle
    (unless (typep position 'V2)
      (setf position (v 0.0 0.0)))))

(defmethod initialize-instance :after ((turtle turtle3d) &key)
  (with-slots (position) turtle
    (unless (typep position 'V3)
      (setf position (v 0.0 0.0 0.0)))))

(defgeneric copy (turtle)
  (:method ((turtle turtle))
    (make-instance 'turtle :position (position turtle)))
  (:method ((turtle turtle2d))
    (make-instance 'turtle2d
                   :position (position turtle)
                   :direction (direction turtle)))
  (:method ((turtle turtle3d))
    (make-instance 'turtle3d
                   :position (position turtle)
                   :space (space turtle))))

(defgeneric head (turtle3d)
  (:method ((turtle3d turtle3d))
    (m-col[] 0 (space turtle3d))))

;; FIXME unary generic method already declared in `definitions.lisp' for `rule'
(defmethod left ((turtle3d turtle3d))
  (m-col[] 1 (space turtle3d)))

(defgeneric up (turtle3d)
  (:method ((turtle3d turtle3d))
    (m-col[] 2 (space turtle3d))))

(defgeneric (setf head) (head turtle3d)
  (:method (head (turtle3d turtle3d))
    (setf (m-col[] 0 (space turtle3d))
          head)))

(defgeneric (setf left) (left turtle3d)
  (:method (left (turtle3d turtle3d))
    (setf (m-col[] 1 (space turtle3d))
          left)))

(defgeneric (setf up) (up turtle3d)
  (:method (up (turtle3d turtle3d))
    (setf (m-col[] 2 (space turtle3d))
          up)))


(defmacro with-3d-turtle-space ((head left up) turtle &body body)
  (once-only (turtle)
    (flet ((binding-for (var f)
             (if var
                 `((,var (,f ,turtle)))
                 ())))
      `(let (,@(binding-for head 'head)
             ,@(binding-for left 'left)
             ,@(binding-for up 'up))
         ,@body))))

(defun space-unit (space)
  "Makes the vectors of a 3D space base unit (yet equivalent) vectors."
  (declare (type (matrix 3 3) space))
  (the (matrix 3 3)
       (m (v-unit (m-row[] 0 space))
          (v-unit (m-row[] 1 space))
          (v-unit (m-row[] 2 space)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Environments

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


(defgeneric save (env filename))

(defgeneric stack (env)
  (:method progn ((env environment))
    (with-slots (turtle) env
      (push turtle (env-stack env))
      (setf turtle (copy turtle))))
  (:method-combination progn :most-specific-last))

(defgeneric unstack (env)
  (:method progn ((env environment))
    (setf (turtle env)
          (pop (env-stack env))))
  (:method-combination progn :most-specific-first))


(defmacro with-updated-turtle ((turtle env) &body body) ; TODO
  (once-only (env)
    `(setf (turtle ,env)
           (let ((,turtle (copy (turtle ,env))))
             ,@body
             ,turtle))))

(defun update-turtle (env &key position
                            direction
                            space head left up)
  (with-updated-turtle (turtle env)
    (when position (setf (position turtle) position))
    (when direction (setf (direction turtle) direction))
    (when space (setf (space turtle) space))
    (when head (setf (head turtle) head))
    (when left (setf (left turtle) left))
    (when up (setf (up turtle) up))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; PNG using VECTO

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
;;;; Wavefront OBJ

(defun sxhash-vect (v)
  (sxhash (map 'list #'identity v)))
(sb-ext:define-hash-table-test v= sxhash-vect)

(defclass obj-environment (3d-environment)
  ((vertices :initform ()
             :accessor vertices)
   (lines :initform ()
          :accessor lines)
   (faces :initform ()
          :accessor faces)
   (current-index :initform 1)))


(defmethod initialize-instance :after ((env obj-environment) &key)
  (add-vertice env (v 0 0 0)))

(defmethod save ((env obj-environment) filename)
  (with-open-file (obj (format nil "~a.obj" filename)
                       :direction :output
                       :if-exists :supersede)
    (with-slots (vertices lines faces) env
      (labels ((dump-v (v)
                 (format obj "~&v ~f ~f ~f" (vx v) (vy v) (vz v)))
               (index-of (v)
                 (1+ (cl:position v vertices :test #'v~)))
               (dump-l (l)
                 (destructuring-bind (src . dst) l
                   (format obj "~&l ~a ~a" (index-of src) (index-of dst))))
               (dump-f (f)
                 (format obj "~&f~{ ~a~}" (mapcar #'index-of f))))
        (mapc #'dump-v vertices)
        (mapc #'dump-f faces)
        (mapc #'dump-l lines))))
  (values))


(defun add-vertice (env v)
  (declare (type V3 v)
           (type obj-environment env))
  (with-slots (vertices) env
    (unless (member v vertices :test #'v~)
      (push v vertices))))
