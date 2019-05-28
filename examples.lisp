(in-package :cl-lsystem)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 2D L-Systems

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

;; equation (1.9) of http://algorithmicbotany.org/papers/abop/abop-ch1.pdf

(let ((R 1.456)
      (K 750.0)
      (delta (deg->rad 85)))
  (define-lsystem *eq-1.9* #w(A 1))
  (define-rule A (s) #i(noop)
    #w((F s) ([) (+) (A (/ s R)) (]) ([) (-) (A (/ s R)) (])))
  (define-rule F (s) #i(forward (* s K)))
  (define-rule [ () #i(stack))
  (define-rule ] () #i(unstack))
  (define-rule + () #i(rotate delta))
  (define-rule - () #i(rotate (- delta))))

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

(define-lsystem *3d-hilbert* #wA)
(define-3d-turtle (/ pi 2))
(define-rule ! () #i(yaw pi)) ; turn around
(define-rule A () #i(noop)
  #wB-F+CFC+F-D&F^D-F+&&CFC+F+B>>)
(define-rule B () #i(noop)
  #wA&F^CFB^F^D^^-F-D^!F^B!FC^F^A>>)
(define-rule C () #i(noop)
  #w!D^!F^B-F+C^F^A&&FA&F^C+F+B^F^D>>)
(define-rule D () #i(noop)
  #w!CFB-F+B!FA&F^A&&FB-F+B!FC>>)

(define-lsystem *figure-1.25* #wA)
(define-rule A () #i(noop)
  #w[&FLA]>>>>>[&FLA]>>>>>>>[&FLA])
(define-rule F () #i(forward 3)
  #wS>>>>>F)
(define-rule S () #i(noop)
  #wFL)
(define-rule L () #i(noop)
  #w[^^{-J+J+J-!-J+J+J}])
(define-rule J () #i(jump 0.5))
(define-rule ! () #i(yaw pi)) ; turn around
(define-rule { () #i(begin-fill))
(define-rule } () #i(end-fill))
(define-rule [ () #i(stack))
(define-rule ] () #i(unstack))
(let ((delta (deg->rad 22.5)))
  (define-rule < () #i(roll delta))
  (define-rule > () #i(roll (- delta)))
  (define-rule ^ () #i(pitch delta))
  (define-rule & () #i(pitch (- delta)))
  (define-rule + () #i(yaw delta))
  (define-rule - () #i(yaw (- delta))))



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
