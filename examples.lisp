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
