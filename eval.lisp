(in-package :cl-lsystem)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Common instructions

(defgeneric eval (instruction env)
  (:documentation "Evaluates the instruction for the given environment."))

(defmethod eval ((noop noop) (env environment))
  (declare (ignore noop env)))

(defmethod eval ((stack stack) (env environment))
  (declare (ignore stack))
  (stack env))

(defmethod eval ((unstack unstack) (env environment))
  (declare (ignore unstack))
  (unstack env))

(defmethod eval ((lisp lisp) (env environment))
  (with-slots (procedure) lisp
    (funcall procedure env)))

(defun move-by-delta (position direction delta)
  (v+ position
      (v* (scalar->v delta (v-dim direction))
          direction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 2D environment

(defmethod eval ((jump jump) (env 2d-environment))
  (with-updated-turtle (turtle env)
    (with-slots (position direction) turtle
      (setf position
            (move-by-delta position direction
                           (with-slots (delta) jump
                             delta))))))

(defun 2d-rotation-matrix (theta)
  (declare (type real theta))
  (let ((m (mat ((cos theta) (sin theta))
                ((- (sin theta)) (cos theta)))))
    (the (matrix 2 2) m)))

(defun rotate-2d-direction (theta d)
  (declare (type real theta)
           (type V2 d))
  (let ((newd (-> d
                  v->m
                  (m* (2d-rotation-matrix theta))
                  m->v
                  v-unit)))
    (the V2 newd)))

(defmethod eval ((rotate rotate) (env 2d-environment))
  (with-slots (theta) rotate
    (declare (type real theta)) ; => in a 2D space, `angle' is only a scalar
    (let* ((turtle (turtle env))
           (newd (rotate-2d-direction theta
                                      (direction turtle))))
      (update-turtle env :direction newd))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 3D environment

(defmethod eval ((jump jump) (env 3d-environment))
  (with-updated-turtle (turtle env)
    (with-slots (position) turtle
      ;; move the turtle
      (setf position
            (move-by-delta position (head turtle)
                           (with-slots (delta) jump
                             delta)))
      ;; if a filling polygon is being built, update it
      (with-slots (fill-stack) env
        (when (car fill-stack)
          (setf fill-stack
                (cons (cons position (car fill-stack))
                      (rest fill-stack))))))))

;;; NOTE roll => R(x), pitch = R(y), yaw = R(z)
(defun roll-3d-rotation-matrix (theta)
  (declare (type real theta))
  (let ((m (mat (1 0 0)
                (0 (cos theta) (- (sin theta)))
                (0 (sin theta) (cos theta)))))
    (the (matrix 3 3) m)))

(defun pitch-3d-rotation-matrix (theta)
  (declare (type real theta))
  (let ((m (mat ((cos theta) 0 (sin theta))
                (0 1 0)
                ((- (sin theta)) 0 (cos theta)))))
    (the (matrix 3 3) m)))

(defun yaw-3d-rotation-matrix (theta)
  (declare (type real theta))
  (let ((m (mat ((cos theta) (- (sin theta)) 0)
                ((sin theta) (cos theta) 0)
                (0 0 1))))
    (the (matrix 3 3) m)))

(defun rotate-3d-space (theta kind space)
  (declare (type real theta)
           (type (member :roll :pitch :yaw) kind)
           (type (matrix 3 3) space))
  (let* ((rho (ecase kind
              (:roll (roll-3d-rotation-matrix theta))
              (:pitch (pitch-3d-rotation-matrix theta))
              (:yaw (yaw-3d-rotation-matrix theta))))
         (newspace (-> (m* space rho)
                      space-unit)))
    (the (matrix 3 3)
         newspace)))

(defun 3d-turn (env theta kind)
  (declare (type 3d-environment env)
           (type real theta)
           (type (member :roll :pitch :yaw) kind))
  (with-updated-turtle (turtle env)
    (with-slots (space) turtle
      (setf space
            (rotate-3d-space theta kind space)))))

(defmethod eval ((roll roll) (env 3d-environment))
  (with-slots (theta) roll
    (3d-turn env theta :roll)))

(defmethod eval ((pitch pitch) (env 3d-environment))
  (with-slots (theta) pitch
    (3d-turn env theta :pitch)))

(defmethod eval ((yaw yaw) (env 3d-environment))
  (with-slots (theta) yaw
    (3d-turn env theta :yaw)))

(defmethod eval ((begin-fill begin-fill) (env 3d-environment))
  (with-slots (fill-stack turtle) env
    ;; push initial position of the turtle when filling begins
    (push (list (position turtle))
          fill-stack)))

(defmethod eval ((end-fill end-fill) (env 3d-environment))
  (with-slots (fill-stack) env
    (pop fill-stack)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; PNG environment

(defmethod eval ((forward forward) (env png-environment))
  (v-bind (oldx oldy)
      (position (turtle env))
    (call-next-method) ; updates the turtle (cf. eval(jump environment))
    (v-bind (newx newy)
        (position (turtle env))
      (eval-in-graphics-state env (lambda ()
                                    (vecto:move-to oldx oldy)
                                    (vecto:line-to newx newy))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; OBJ environment

(defun circle-in-plan (u v &optional (n 16))
  "Returns a list containing the coordinates of the points a circle
at origin in the plan (`u', `v'). `u' and `v' must be unit orthogonal vectors.
`n' is the number of points to generate."
  (declare (type vect u v)
           (type (integer (0) *) n))
  (flet ((point-at (theta)
           (declare (type float theta))
           (v+ (v* u (scalar->v (cos theta)
                                (v-dim u)))
               (v* v (scalar->v (sin theta)
                                (v-dim v))))))
    (loop :with dtheta := (/ (* 2 pi) n)
          :for i :from 0 :below n
          :for theta := (* dtheta i)
          :collect (point-at theta))))

(defun make-cylinder (circle-1 circle-2 &optional (n 16))
  (flet ((make-circular (list)
           (let ((clist (copy-seq list)))
             (setf (cdr (last clist))
                   clist)
             clist)))
    (loop :repeat n
          :for f1 := (make-circular circle-1) :then (cdr f1)
          :for f2 := (make-circular circle-2) :then (cdr f2)
          :for a := (first f1)
          :for b := (second f1)
          :for alpha := (first f2)
          :for beta := (second f2)
          ;; the order of the vertices in the face is important for OBJ's normal interpolation
          :collecting (list a b alpha)
          :collecting (list alpha b beta))))

(defmethod eval ((forward forward) (env obj-environment))
  (let* ((turtle (turtle env))
         (oldp (position turtle)))
    (call-next-method) ; updates the turtle's position
    (with-3d-turtle-space (nil v w) turtle
      (flet ((scale-translate-vertices (delta lambda vertices)
               "Scales each vertice of `vertices' by a factor `delta' and then
translates them by `lambda' which is a 3D vector. Returns the list of the new vertices."
               (declare (type V3 lambda)
                        (type real delta))
               (mapcar (lambda (v)
                         (v+ lambda (v* v (scalar->v delta (v-dim v)))))
                       vertices)))
        (with-slots (turtle branch-radius branch-decay edges-per-branch) env
          (let* ((newp (position turtle))
                 (circle (circle-in-plan v w edges-per-branch))
                 (c1 (scale-translate-vertices branch-radius oldp circle))
                 (new-radius (* branch-radius branch-decay))
                 (c2 (scale-translate-vertices new-radius newp circle)))

            ;; update environment's branch radius
            (setf branch-radius new-radius)

            ;; add the vertices of the circles
            (flet ((add (vs)
                     (dolist (v vs)
                       (add-vertice env v))))
              (add c1)
              (add c2))

            ;; the circles (convex shapes) are within the same plan => one single face
            (add-face env c1)
            (add-face env c2)

            ;; the body of the cylinder
            (mapc (lambda (f)
                    (add-face env f))
                  (make-cylinder c1 c2 edges-per-branch))))))))

(defmethod eval ((end-fill end-fill) (env obj-environment))
  (with-slots (fill-stack) env
    (let ((polygon (-<> (first fill-stack) ; the polygon is the top of the stack
                        ;; reversed because of the head insertion of voxels
                        (reverse <>)
                        ;; depending on the path of the turtle, its last position may also
                        ;; be its first, so we need to remove that eventual duplicate
                        ;; because of OBJ format
                        (remove-duplicates <> :test #'v~))))
      (assert polygon nil "weird, and WTF? the L-System might be wrong")
      ;; ensure every vertice is added (non-printing jumps might have occured)
      (mapc (lambda (v)
              (add-vertice env v))
            polygon)
      ;; finally add the face (NOTE the writter of the L-System is responsible
      ;; of normal interpolation correctness)
      (add-face env polygon)))
  (call-next-method))

(defmethod eval ((apply-material apply-material) (env obj-environment))
  (with-slots (material) apply-material
    (let ((new-group (make-obj-face-tree :material material)))
      (add-subgroup env new-group))))

(defmethod eval ((pop-material pop-material) (env obj-environment))
  (declare (ignore pop-material))
  (pop-face-stack env))
