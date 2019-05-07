(in-package :cl-lsystem)

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


(defun 2d-rotation-matrix (theta)
  (declare (type real theta))
  (let ((m (mat ((cos theta) (sin theta))
                ((- (sin theta)) (cos theta)))))
  (the (matrix 2 2) m)))

(defun rotate-2D-direction (theta d)
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
           (newd (rotate-2D-direction angle
                                      (direction turtle))))
      (update-turtle env :direction newd))))


#|
{{
H Cos[b] Cos[c] + L (-(Cos[c] Sin[a] Sin[b]) - Cos[a] Sin[c]) + U (Cos[a] Cos[c] Sin[b] - Sin[a] Sin[c]),
H Cos[b] Sin[c] + U (Cos[c] Sin[a] + Cos[a] Sin[b] Sin[c]) + L (Cos[a] Cos[c] - Sin[a] Sin[b] Sin[c]),
U Cos[a] Cos[b] - L Cos[b] Sin[a] - H Sin[b]
}}
|#
(defun oriented-delta (orientation delta)
  (v-bind (h l u) delta
    (v-bind (a b c) orientation
      (v (+ (* H (cos b) (cos c))
            (* L (- (- (* (cos c) (sin a) (sin b)))
                    (* (cos a) (sin c))))
            (* U (- (* (cos a) (cos c) (sin b))
                    (* (sin a) (sin c)))))
         (+ (* H (cos b) (sin c))
            (* U (+ (* (cos c) (sin a))
                    (* (cos a) (sin b) (sin c))))
            (* L (- (* (cos a) (cos c))
                    (* (sin a) (sin b) (sin c)))))
         (- (* U (cos a) (cos b))
            (* L (cos b) (sin a))
            (* H (sin b)))))))

(defun forward-pos (turtle delta)
  (with-slots (position direction) turtle
    (v+ position
        (v* (scalar->v delta (v-dim direction))
            direction))))

(defmethod eval ((fwd forward) (env png-environment))
  (with-slots (delta) fwd
    (let* ((turtle (turtle env))
           (oldp (position turtle))
           (newp (forward-pos turtle delta)))
      (update-turtle env :position newp)
      (eval-in-graphics-state env (lambda ()
                                    (vecto:move-to (vx oldp) (vy oldp))
                                    (vecto:line-to (vx newp) (vy newp)))))))

(defmethod eval ((fwd forward) (env obj-environment))
  (with-slots (delta) fwd
    (let* ((turtle (turtle env))
           (oldp (position turtle))
           (newp (forward-pos turtle delta)))
      (update-turtle env :position newp)
      (with-slots (vertices lines current-index) env
        (setf (gethash newp vertices) current-index)
        (incf current-index)
        (push (cons oldp newp) lines)))))

(defmethod eval ((fwd forward) (env vrml-environment))
  (with-slots (delta) fwd
    (let* ((turtle (turtle env))
           (oldp (position turtle))
           (newp (forward-pos turtle delta))
           (diffp (v- newp oldp)))
      (update-turtle env :position newp)
      (let* ((branch (make-instance 'vrml-shape
                                    :geometry (make-instance 'vrml-cylinder
                                                             :height delta)))

             (translation (make-instance 'vrml-translation
                                         :vect (v* (v 0.5 0.5 0.5)
                                                   diffp)
                                         :children (list branch))))
        (with-slots (transform) env
          (with-slots (children) transform
            (setf children (append children
                                   (list (make-instance 'vrml-shape
                                                        :geometry translation))))
            (setf transform translation)))))))
