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

(defmethod eval ((turn turn) (env 2d-environment))
  (with-slots (angle) turn
    (declare (type real angle)) ; => in a 2D space, `angle' is only a scalar
    (let* ((turtle (turtle env))
           (newd (rotate-2d-direction angle
                                      (direction turtle))))
      (update-turtle env :direction newd))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 3D environment

(defmethod eval ((jump jump) (env 3d-environment))
  (with-updated-turtle (turtle env)
    (with-slots (position) turtle
      (setf position
            (move-by-delta position (head turtle)
                           (with-slots (delta) jump
                             delta))))))

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

(defun rotation-vector->rotation-angle-and-kind (v &optional (epsilon 0.0000001))
  (declare (type V3 v)
           (type (float (0.0) *) epsilon))
  (flet ((~zerop (number) ; read "approximately zerop" :D
           (<= (- epsilon) number epsilon)))
    (let ((non-zero-indexes (remove-if #'~zerop '(0 1 2)
                                       :key (lambda (i)
                                              (v[] i v)))))
      (cond
        ((null non-zero-indexes)
         (error "if you want to rotate, why is your vector ~a null, you dumba**?" v))
        ((null (rest non-zero-indexes))
         (let ((i (first non-zero-indexes)))
           (values (v[] i v)
                   (nth i '(:roll :pitch :yaw)))))
        (t
         (error "cannot infer rotation axis when the rotation vector ~a has more that one non-zero component" v))))))

(defmethod eval ((turn turn) (env 3d-environment))
  (with-slots (angle) turn
    (declare (type V3 angle)) ; => in a 3D space, `angle' must be a 3D vector [roll pitch yaw]
    (with-updated-turtle (turtle env)
      (multiple-value-bind (theta kind)
          (rotation-vector->rotation-angle-and-kind angle)
        (with-slots (space) turtle
          (setf space
                (rotate-3d-space theta kind space)))))))


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

(defmethod eval ((forward forward) (env obj-environment))
  (let* ((turtle (turtle env))
         (oldp (position turtle)))
    (call-next-method) ; updates the turtle's position
    (let ((newp (position (turtle env))))
      (add-vertice env newp)
      (add-vertice env oldp) ; ensure last position has its vertice (last inst may have been a jump)
      (with-3d-turtle-space (nil v w) turtle
        (let* ((face (list (v+ v w)
                           (v+ (v- v) w)
                           (v- w)))
               (base-face (mapcar (lambda (vertice)
                                    (v+ vertice oldp))
                                  face))
               (other-face (mapcar (lambda (vertice)
                                    (v+ vertice newp))
                                  face)))
          (with-slots (faces) env
            (destructuring-bind ((a b c) . (alpha beta gamma))
                (cons base-face other-face)
              (push (list a b beta alpha) faces)
              (push (list c a alpha gamma) faces)
              (push (list b c gamma beta) faces)
              (mapc (lambda (vertice)
                      (add-vertice env vertice))
                    (list a b c alpha beta gamma)))
            (push base-face faces)
            (push other-face faces)))))))
