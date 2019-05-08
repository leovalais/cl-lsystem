(in-package :cl-lsystem)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 2D L-Systems

(defparameter *koch*
  '(:axiom (F)
    :rules ((F (F + F - F - F + F)))
    :turtle ((F (forward 5))
             (+ (turn (/ pi 2)))
             (- (turn (- (/ pi 2)))))))

(defparameter *siertri*
  '(:axiom (Fr)
    :rules ((F1 (Fr + F1 + Fr))
            (Fr (F1 - Fr - F1)))
    :turtle ((Fr (forward 5))
             (F1 (forward 5))
             (+ (turn (/ pi 3)))
             (- (turn (- (/ pi 3)))))))

;; plants in http://algorithmicbotany.org/papers/abop/abop-ch1.pdf figure 1.24

(defparameter *plant-p-delta* (/ pi 9))
(defparameter *plant-p*
  '(:axiom (X)
    :rules ((X (F [ + X ] F [ - X ] + X))
            (F (F F)))
    :initial-orientation (/ pi 2)
    :turtle ((X (forward 5))
             (F (forward 5))
             (+ (turn *plant-p-delta*))
             (- (turn (- *plant-p-delta*)))
             ([ (stack))
             (] (unstack)))))


(defun deg->rad (x)
  (* x (/ pi 180)))

(defparameter *plant-f-delta* (deg->rad 22.5))
(defparameter *plant-f*
  '(:axiom (X)
    :rules ((X (F - [ [ X ] + X ] + F [ + F X ] - X))
            (F (F F)))
    :initial-orientation (/ pi 2)
    :turtle ((X (forward 5))
             (F (forward 5))
             (+ (turn *plant-f-delta*))
             (- (turn (- *plant-f-delta*)))
             ([ (stack))
             (] (unstack)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 3D L-Systems

(defun standard-3d-turtle (delta)
  `((F (forward 3))
    ([ (stack))
    (] (unstack))
    (< (turn (v ,delta 0 0)))
    (> (turn (v (- ,delta) 0 0)))
    (^ (turn (v 0 ,delta 0)))
    (& (turn (v 0 (- ,delta) 0)))
    (+ (turn (v 0 0 ,delta)))
    (- (turn (v 0 0 (- ,delta))))))

;; trees in http://www.geekyblogger.com/2008/04/tree-and-l-system.html

(defparameter *tree-a-delta* (deg->rad 25))
(defparameter *tree-a*
  `(:axiom (F F F F F A)
    :rules ((A (F [ + + A L ] [ - - A L ] > > > A)))
    :initial-orientation (v 0 0 (/ pi 2))
    :turtle (,@(standard-3d-turtle *tree-a-delta*)
             (L (forward 2))
             (A (noop))
             (B (noop)))))

(defparameter *tree-b-delta* (deg->rad 30))
(defparameter *tree-b*
  `(:axiom (F A)
    ;;  A = f[^Bl]>>[^Bl]>>A, B = f[-Bl]B
    :rules ((A (F [ ^ B L ] > > [ ^ B L ] > > A))
            (B (F [ - B L ] B)))
    :turtle (,@(standard-3d-turtle *tree-b-delta*)
             (L (forward 5))
             (A (noop))
             (B (noop)))))

(defparameter *tree-c-delta* (deg->rad 15))
(defparameter *tree-c*
  `(:axiom (F A)
    :rules ((A (^ F B > > > B > > > > > B))
            (B ([ ^ ^ F > > > > > > A ])))
    ;; :initial-orientation (v 0 0 (/ pi 2))
    :turtle (,@(standard-3d-turtle *tree-c-delta*)
             (A (noop))
             (B (noop)))))

(defparameter *tree-test-delta* (deg->rad 20))
(defparameter *tree-test*
  `(:axiom (A)
    :rules ((A (F [ & B ] [ ^ B ] [ + B ] [ - B ]))
            (B (F A)))
    ;; :initial-orientation (v 0 0 (/ pi 2))
    :turtle (,@(standard-3d-turtle *tree-test-delta*)
             (A (noop))
             (B (noop)))))
