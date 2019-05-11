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


(defun normal-plan-base (u)
  "Returns the unit vectors (v, w) such as (u, v, w) is a base of R^3. u /= 0."
  (multiple-value-bind (v w)
      (trivia:match u
        ;; u is null => never happens... theoretically at least :'\
        ((vector 0 0 0)
         (error "normal-plan-base: u cannot be null"))

        ;; two coordinates are null => the base is the unit vectors of the other axes
        ((vector _ 0 0) (values (v 0 1 0)
                                (v 0 0 1)))
        ((vector _ 0 0) (values (v 0 1 0)
                                (v 0 0 1)))
        ((vector 0 _ 0) (values (v 1 0 0)
                                (v 0 0 1)))
        ((vector 0 0 _) (values (v 1 0 0)
                                (v 0 1 0)))

        ;; only one coordinate is null => it's just like computing the a normal vector
        ;; in the 2D plane => n = (-b a)
        ;; the other (3D) normal vector w is just the unit vector of the axis orthogonal
        ;; to the (u, v) plane
        ((vector a b 0) (values (v (- b) a 0)
                                (v     0 0 1)))
        ((vector a 0 b) (values (v (- b) 0 a)
                                (v     0 1 0)))
        ((vector 0 a b) (values (v 0 (- b) a)
                                (v 1     0 0)))

        #|
In the general case, given a vector $\vec u = (a\ b\ c)$, we want to find the vectors $(\vec v, \vec w)$ such as
$(\vec u, \vec v, \vec w)$ is a base of the 3D space ($\mathbb R^3$). I.e.:
$$\vec u \cdot \vec v = 0$$
$$\vec u \cdot \vec w = 0$$
$$\vec v \cdot \vec w = 0$$

Trivially, we can find that $\vec v = (-b\ a\ 0)$ is a possible value that satisfies the first equation
$\vec u \cdot \vec v = \vec 0$. Proof:
\begin{align*}
\vec u \cdot \vec v &= a \times (-b) + b \times a + c \times 0\\
                    &= 0
\end{align*}

For $\vec w$ though, we need to find coordinates which both satisfies the constraints $\vec u \cdot \vec w = 0$
and $\vec v \cdot \vec w = 0$. By fiddling a little with $a$, $b$ and $c$, we can prove that
$\vec w = (-c \cdot a\quad -c \cdot b\quad a^2 + b^2)$ is a valid vector:
\begin{align*}
\vec u \cdot \vec w &= a \times (-c \cdot a) + b \times (-c \cdot b) + c \times (a^2 + b^2)\\
                    &= -a^2 \times c - b^2 \times c + c \times (a^2 + b^2)\\
                    &= c \times (-a^2 - b^2 + a^2 + b^2)\\
                    &= c \times 0\\
                    &= 0
\end{align*}
\begin{align*}
\vec v \cdot \vec w &= -b \times (-c \cdot a) + a \times (-c \cdot b) + 0 \times (a^2 + b^2)\\
                    &= a \cdot b \cdot c - a \cdot b \cdot c\\
                    &= 0
\end{align*}
|#
        ((vector a b c) (values (v (- b) a 0)
                                (v (* (- c) a)
                                   (* (- c) b)
                                   (+ (* a a) (* b b))))))
    (flet ((~zerop (x &optional (epsilon 0.000000001))
             (<= (- epsilon) x epsilon)))
      (assert (~zerop (v^ u v)))
      (assert (~zerop (v^ u w)))
      (assert (~zerop (v^ v w))))
    (the (values V3 V3)
         (values v w))))

(defmethod eval ((forward forward) (env obj-environment))
  (let* ((turtle (turtle env))
         (oldp (position turtle))
         (u (direction turtle)))
    (call-next-method) ; updates the turtle's position
    (let ((newp (position (turtle env))))
      (add-vertice env newp)
      (add-vertice env oldp) ; ensure last position has its vertice (last inst may have been a jump)
      (multiple-value-bind (v w)
          (normal-plan-base u)
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
