(in-package :cl-lsystem)

(defparameter *koch*
  '(:axiom (F)
    :rules ((F (F + F - F - F + F)))
    :turtle ((F (forward 5))
             (+ (turn (v 0 0 (/ pi 2))))
             (- (turn (v 0 0 (- (/ pi 2))))))))

(defparameter *siertri*
  '(:axiom (Fr)
    :rules ((F1 (Fr + F1 + Fr))
            (Fr (F1 - Fr - F1)))
    :turtle ((Fr (forward 5))
             (F1 (forward 5))
             (+ (turn (v 0 0 (/ pi 3))))
             (- (turn (v 0 0 (- (/ pi 3))))))
    :initial-orientation (v 0 0 (* -1/3 pi))))

;; plants in http://algorithmicbotany.org/papers/abop/abop-ch1.pdf figure 1.24

(defparameter *plant-p-delta* (/ pi 9))
(defparameter *plant-p*
  '(:axiom (X)
    :rules ((X (F [ + X ] F [ - X ] + X))
            (F (F F)))
    :initial-orientation (v 0 0 (/ pi 4))
    :turtle ((X (forward 5))
             (F (forward 5))
             (+ (turn (v 0 0 *plant-p-delta*)))
             (- (turn (v 0 0 (- *plant-p-delta*))))
             ([ (stack))
             (] (unstack)))))


(defun deg->rad (x)
  (* x (/ pi 180)))

(defparameter *plant-f-delta* (deg->rad 22.5))
(defparameter *plant-f*
  '(:axiom (X)
    :rules ((X (F - [ [ X ] + X ] + F [ + F X ] - X))
            (F (F F)))
    :initial-orientation (v 0 0 (/ pi 4))
    :turtle ((X (forward 5))
             (F (forward 5))
             (+ (turn (v 0 0 *plant-f-delta*)))
             (- (turn (v 0 0 (- *plant-f-delta*))))
             ([ (stack))
             (] (unstack)))))


;; trees in http://www.geekyblogger.com/2008/04/tree-and-l-system.html

(defparameter *tree-b-delta* (deg->rad 30))
(defparameter *tree-b*
  '(:axiom (F A)
    ;;  A = f[^Bl]>>[^Bl]>>A, B = f[-Bl]B
    :rules ((A (F [ ^ B L ] > > [ ^ B L ] > > A))
            (B (F [ - B L ] B)))
    :turtle ((F (forward 5))
             (L (forward 5))
             (A (noop))
             (B (noop))
             ([ (stack))
             (] (unstack))
             (+ (turn (v 0 0 *tree-b-delta*)))
             (- (turn (v 0 0 (- *tree-b-delta*))))
             (^ (turn (v 0 *tree-b-delta* 0)))
             (& (turn (v 0 (- *tree-b-delta*) 0)))
             (< (turn (v *tree-b-delta* 0 0)))
             (> (turn (v (- *tree-b-delta*) 0 0))))))
