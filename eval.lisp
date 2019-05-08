(in-package :cl-lsystem)


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


(defun move-by-delta (turtle delta)
  (with-slots (position direction) turtle
    (v+ position
        (v* (scalar->v delta (v-dim direction))
            direction))))

(defmethod eval ((jump jump) (env environment))
  (assert (typep env '(or 2d-environment 3d-environment)))
  (let* ((turtle (turtle env))
         (newp (move-by-delta turtle
                              (with-slots (delta) jump
                                delta))))
    (update-turtle env :position newp)))

;;;; Turn

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

(defun rotate-3d-direction (theta d kind)
  (declare (type real theta)
           (type V3 d)
           (type (member :roll :pitch :yaw) kind))
  (let* ((matrix (ecase kind
                   (:roll (roll-3d-rotation-matrix theta))
                   (:pitch (pitch-3d-rotation-matrix theta))
                   (:yaw (yaw-3d-rotation-matrix theta))))
         (newd (-> d
                  v->m
                  (m* matrix)
                  m->v
                  v-unit)))
    (the V3 newd)))

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
    (let ((d (direction (turtle env))))
      (multiple-value-bind (theta kind)
          (rotation-vector->rotation-angle-and-kind angle)
        (let ((newd (rotate-3d-direction theta d kind)))
          (update-turtle env :direction newd))))))


;;;; Forward

(defmethod eval ((forward forward) (env png-environment))
  (v-bind (oldx oldy)
      (position (turtle env))
    (call-next-method) ; updates the turtle (cf. eval(jump environment))
    (v-bind (newx newy)
        (position (turtle env))
      (eval-in-graphics-state env (lambda ()
                                    (vecto:move-to oldx oldy)
                                    (vecto:line-to newx newy))))))

(defmethod eval ((forward forward) (env obj-environment))
  (let ((oldp (position (turtle env))))
    (call-next-method) ; updates the turtle's position
    (let ((newp (position (turtle env))))
      (add-vertice env newp)

      ;; draw a line
      (add-vertice env oldp) ; ensure last position has its vertice (last inst may have been a jump)
      (push (cons oldp newp)
            (lines env)))))

(defmethod eval ((forward forward) (env vrml-environment))
  (let ((oldp (position (turtle env))))
    (call-next-method)
    (let* ((newp (position (turtle env)))
           (diffp (v- newp oldp)))
      (with-slots (delta) forward
        (let* ((material (make-instance 'vrml-material
                                        :diffuse (if (= delta 5)
                                                     (v 0.3 0.3 0.3)
                                                     (v 0.1 0.7 0.2))))
               (branch (make-instance 'vrml-shape
                                      :material material
                                      :geometry (make-instance 'vrml-cylinder
                                                               :radius 0.1
                                                               :height delta)))
               (translation (make-instance 'vrml-translation
                                           :vect diffp
                                           :children (list branch))))
          (with-slots (transform) env
            (with-slots (children) transform
              (appendf children (list (make-instance 'vrml-shape
                                                     :geometry translation)))
              (setf transform translation))))))))

(defmethod eval ((turn turn) (env vrml-environment))
  (call-next-method) ; updates the turtle
  (with-slots ((theta angle)) turn
    (declare (type V3 theta))
    (let ((rotation (make-instance 'vrml-rotation :vect theta)))
      (with-slots (transform) env
        (with-slots (children) transform
          (appendf children (list rotation))
          (setf transform rotation))))))
