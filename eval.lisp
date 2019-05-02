(in-package :cl-lsystem)

(defgeneric eval (instruction env)
  (:documentation "Evaluates the instruction for the given environment."))

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

(defmethod eval ((noop noop) (env environment))
  (declare (ignore noop env)))

(defmethod eval ((fwd forward) (env png-environment))
  (with-slots (delta) fwd
    (let* ((turtle (turtle env))
           (oldp (turtle-position turtle))
           (newp (v+ oldp (oriented-delta (turtle-orientation turtle)
                                          (v delta delta delta)))))
      (update-turtle env :position newp)
      (eval-in-graphics-state env (lambda ()
                                    (vecto:move-to (vx oldp) (vy oldp))
                                    (vecto:line-to (vx newp) (vy newp)))))))

(defmethod eval ((turn turn) (env png-environment))
  (with-slots (angle) turn
    (let ((turtle (turtle env)))
      (update-turtle env :orientation (v+ (turtle-orientation turtle)
                                          angle)))))

(defmethod eval ((stack stack) (env environment))
  (declare (ignore stack))
  (stack env))

(defmethod eval ((unstack unstack) (env environment))
  (declare (ignore unstack))
  (unstack env))
