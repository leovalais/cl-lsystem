(in-package :cl-lsystem)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Turtles

(defclass turtle ()
  ((position :initform nil
             :initarg :position
             :accessor position
             :type (or null vect))))

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

(defgeneric left (turtle3d)
  (:method ((turtle3d turtle3d))
    (m-col[] 1 (space turtle3d))))

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
           :initarg :turtle
           :type turtle2D
           :accessor turtle)))

(defclass 3d-environment (environment)
  ((turtle :initform (make-instance 'turtle3D)
           :initarg :turtle
           :type turtle3D
           :accessor turtle)
   (branch-radius :initform 1.0
                  :initarg :branch-radius
                  :accessor branch-radius
                  :type float)
   (branch-decay :initform 1.0 ; no decay
                 :initarg :branch-decay
                 :accessor branch-decay
                 :type (float (0.0) 1.0))
   (fill-stack :initform ()
               :accessor fill-stack
               ;; list of polygons (i.e.: list of voxels)
               :type (rte:rte (:* (rte:rte (:* V3)))))))


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

(deftype face ()
  '(rte:rte (:cat V3 V3 V3 (:* V3))))

(defclass obj-environment (3d-environment)
  ((vertices :initform (make-array 16 :element-type 'V3
                                      :initial-element (v 0 0 0)
                                      :adjustable t
                                      :fill-pointer 1)
             :accessor vertices
             :type (array V3 1))
   (lines :initform ()
          :accessor lines
          :type list)
   (faces-root :initform (make-obj-face-tree :group-name :root)
               :accessor faces-root
               :type obj-face-tree)
   (faces-stack :initform ()
                :accessor faces-stack
                :type (rte:rte (:* face)))
   (edges-per-branch :initform 16
                     :initarg :edges-per-branch
                     :reader edges-per-branch
                     :type (integer (0) *))))

(defstruct (obj-face-tree (:conc-name oft-))
  (group-name (gensym "OBJ-FACE-TREE-GROUP-NAME")
   :type (or (and symbol (not keyword))
             (eql :root)))
  (material nil :type (or null obj-material))
  (faces (make-array 16 :element-type '(or face null)
                        :initial-element ()
                        :adjustable t
                        :fill-pointer 0)
   :type (array face 1))
  (subgroups (make-array 4 :element-type '(or null obj-face-tree)
                           :initial-element nil
                           :adjustable t
                           :fill-pointer 0)
   :type (array obj-face-tree 1)))

(defclass obj-material ()
  ((name :initform (gensym "MATERIAL")
         :initarg :name
         :reader name
         :type (and symbol (not keyword)))
   (diffuse :initform (v 1.0 1.0 1.0)
            :initarg :diffuse
            :accessor diffuse
            :type (vect 3 (float 0.0 1.0)))))


(defmethod initialize-instance :after ((env obj-environment) &key)
  (push (faces-root env)
        (faces-stack env)))

(defmethod stack progn ((env obj-environment))
  (with-slots (stack) env
    (push (branch-radius env)
          stack)
    (push (branch-decay env)
          stack)))

(defmethod unstack progn ((env obj-environment))
  (with-slots (stack) env
    (setf (branch-decay env)
          (pop stack))
    (setf (branch-radius env)
          (pop stack))))

(defmethod save ((env obj-environment) filename)
  (with-open-file (obj (format nil "~a.obj" filename)
                       :direction :output
                       :if-exists :supersede)
    (with-slots (lines faces-root) env
      (let* ((vertices (delete-duplicates (vertices env)
                                          :test #'v~))
             (indexes (make-hash-table :size (length vertices))))
        (labels ((dump-v (v)
                   (format obj "~&v ~f ~f ~f" (vx v) (vy v) (vz v)))
                 (index-of (v)
                   (or (gethash v indexes)
                       (setf (gethash v indexes)
                             (1+ (cl:position v vertices :test #'v~)))))
                 (dump-l (l)
                   (destructuring-bind (src . dst) l
                     (format obj "~&l ~a ~a" (index-of src) (index-of dst))))
                 (dump-f (f &optional (n 0))
                   (format obj "~&~v{~a~:*~}f~{ ~a~}"
                           n '("  ") (mapcar #'index-of f))))

          (loop :for v :across vertices
                :do (dump-v v))

          (let ((materials ()))
            (labels ((walk (oft n)
                       (format obj "~&~v{~a~:*~}~a ~a" n '("  ")
                               (if (zerop n) "object" "group")
                               (symbol-name (oft-group-name oft)))
                       (incf n)
                       (when-let (mat (oft-material oft))
                         (pushnew mat materials)
                         (format obj "~&~v{~a~:*~}usemtl ~a" n '("  ") (name mat)))
                       (loop :for f :across (oft-faces oft)
                             :do (dump-f f n))
                       (loop :for g :across (oft-subgroups oft)
                             :do (walk g n))))
              (walk faces-root 0))

            (when materials
              (with-open-file (mtl (format nil "~a.mtl" filename)
                                   :direction :output
                                   :if-exists :supersede)
                (mapc (lambda (mat)
                        (save-mtl mtl mat))
                      materials))))

          (mapc #'dump-l lines)))))
  (values))

(defun save-mtl (mtl obj-material)
  (with-slots (name diffuse) obj-material
    (format mtl "~&newmtl ~a
Ns 96.078431
Ka 1.000000 1.000000 1.000000
Kd ~f ~f ~f
Ks 0.500000 0.500000 0.500000
Ke 0.000000 0.000000 0.000000
Ni 1.000000
d 1.000000
illum 2"
            name (vx diffuse) (vy diffuse) (vz diffuse))))

(defun add-vertice (env v)
  (declare (type V3 v)
           (type obj-environment env))
  (vector-push-extend v (vertices env)))

(defun add-face (env f)
  (declare (type face f)
           (type obj-environment env))
  (let ((node (first (faces-stack env))))
    (assert node nil "fucking fuck")
    (vector-push-extend f (oft-faces node))))

(defun add-subgroup (env oft)
  (declare (type obj-face-tree oft)
           (type obj-environment env))
  (let ((node (first (faces-stack env))))
    (assert node nil "fucking fuck")
    (vector-push-extend oft (oft-subgroups node))
    (push oft (faces-stack env))))

(defun pop-face-stack (env)
  (declare (type obj-environment env))
  (unless (rest (faces-stack env))
    (assert (eql (oft-group-name (first (faces-stack env))) :root) nil "uh-oh")
    (error "cannot pop the root of the obj-face-tree ; check your L-System definition"))
  (pop (faces-stack env)))
