(in-package :cl-lsystem)

(defun deg->rad (x)
  (* x (/ pi 180)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 2D L-Systems

(define-lsystem *koch* #wF)
(define-rule F () #i(forward 5) #wF+F-F-F+F)
(define-rule + () #i(rotate (/ pi 2)))
(define-rule - () #i(rotate (/ pi -2)))

(define-lsystem *siertri* #wFr)
(define-rule F1 () #i(forward 5) #wFr+F1+Fr)
(define-rule Fr () #i(forward 5) #w(F1-Fr-F1))
(define-rule + () #i(rotate (/ pi 3)))
(define-rule - () #i(rotate (/ pi -3)))

;; plants in http://algorithmicbotany.org/papers/abop/abop-ch1.pdf figure 1.24

(define-lsystem *plant-p* #wX)
(define-rule X () #i(forward 5) #wF[+X]F[-X]+X)
(define-rule F () #i(forward 5) #wFF)
(define-rule + () #i(rotate (/ pi 9)))
(define-rule - () #i(rotate (/ pi -9)))
(define-rule [ () #i(stack))
(define-rule ] () #i(unstack))

(define-lsystem *plant-p* #wX)
(define-rule X () #i(forward 5) #wF-[[X]+X]+F[+FX]-X)
(define-rule F () #i(forward 5) #wFF)
(define-rule + () #i(rotate (deg->rad 22.5)))
(define-rule - () #i(rotate (deg->rad 22.5)))
(define-rule [ () #i(stack))
(define-rule ] () #i(unstack))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 3D L-Systems

;; trees in http://www.geekyblogger.com/2008/04/tree-and-l-system.html

(defun define-3d-turtle (theta &optional (delta 3))
  (define-rule F () #i(forward delta))
  (define-rule G () #i(jump delta))
  (define-rule [ () #i(stack))
  (define-rule ] () #i(unstack))
  (define-rule < () #i(roll theta))
  (define-rule > () #i(roll (- theta)))
  (define-rule ^ () #i(pitch theta))
  (define-rule & () #i(pitch (- theta)))
  (define-rule + () #i(yaw theta))
  (define-rule - () #i(yaw (- theta))))

(define-lsystem *tree-a* #wFA)
(define-3d-turtle (deg->rad 30) 5)
(define-rule A () #i(noop) #wF[^BL]>>[^BL]>>A)
(define-rule B () #i(noop) #wF[-BL]B)

(define-lsystem *tree-b* #wFA)
(define-3d-turtle (deg->rad 30) 5)
(define-rule A () #i(noop) #wF[^BL]>>[^BL]>>A)
(define-rule B () #i(noop) #wF[-BL]B)

(define-lsystem *tree-c* #wFA)
(define-3d-turtle (deg->rad 15) 5)
(define-rule A () #i(noop) #w^FB>>>B>>>>>B)
(define-rule B () #i(noop) #w^^F>>>>>>A)

(define-lsystem *tree-test* #wA)
(define-3d-turtle (/ pi 3) 3)
(define-rule A () #i(noop) #wF[&B][^B][+B][-B])
(define-rule B () #i(noop) #wFA)


(define-lsystem *param-1.7* #w((B 2) (A 4 4)))
(define-rule A (x y) #i(forward (+ x y))
  (if (<= y 3)
      #w(A (* 2 x)
           (+ x y))
      #w((B x) (A (/ x y) 0))))
(define-rule B (x) #i(jump x)
  (with-disjoint-outcomes
    (9/10 #w(B (1- x)))
    (1/10 #w(B 0))))
