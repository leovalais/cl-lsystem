(in-package :cl-lsystem)

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
