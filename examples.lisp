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
    :initial-orientation (v (/ pi 4) (/ pi 4) (* -1/3 pi))))
