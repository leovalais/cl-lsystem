(in-package :cl-lsystem)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;; 2D L-Systems

;; (defparameter *koch*
;;   '(:axiom (F)
;;     :rules ((F (F + F - F - F + F)))
;;     :turtle ((F (forward 5))
;;              (+ (turn (/ pi 2)))
;;              (- (turn (- (/ pi 2)))))))

;; (defparameter *siertri*
;;   '(:axiom (Fr)
;;     :rules ((F1 (Fr + F1 + Fr))
;;             (Fr (F1 - Fr - F1)))
;;     :turtle ((Fr (forward 5))
;;              (F1 (forward 5))
;;              (+ (turn (/ pi 3)))
;;              (- (turn (- (/ pi 3)))))))

;; ;; plants in http://algorithmicbotany.org/papers/abop/abop-ch1.pdf figure 1.24

;; (defparameter *plant-p-delta* (/ pi 9))
;; (defparameter *plant-p*
;;   '(:axiom (X)
;;     :rules ((X (F [ + X ] F [ - X ] + X))
;;             (F (F F)))
;;     :initial-orientation (/ pi 2)
;;     :turtle ((X (forward 5))
;;              (F (forward 5))
;;              (+ (turn *plant-p-delta*))
;;              (- (turn (- *plant-p-delta*)))
;;              ([ (stack))
;;              (] (unstack)))))


;; (defparameter *plant-f-delta* (deg->rad 22.5))
;; (defparameter *plant-f*
;;   '(:axiom (X)
;;     :rules ((X (F - [ [ X ] + X ] + F [ + F X ] - X))
;;             (F (F F)))
;;     :initial-orientation (/ pi 2)
;;     :turtle ((X (forward 5))
;;              (F (forward 5))
;;              (+ (turn *plant-f-delta*))
;;              (- (turn (- *plant-f-delta*)))
;;              ([ (stack))
;;              (] (unstack)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 3D L-Systems

;; trees in http://www.geekyblogger.com/2008/04/tree-and-l-system.html

(defun deg->rad (x)
  (* x (/ pi 180)))

(defun define-3d-turtle (theta &optional (delta 3))
  (define-rule F () #i(forward delta))
  (define-rule G () #i(jump delta))
  (define-rule [ () #i(stack))
  (define-rule ] () #i(unstack))
  (define-rule < () #i(turn (v theta 0 0)))
  (define-rule > () #i(turn (v (- theta) 0 0)))
  (define-rule ^ () #i(turn (v 0 theta 0)))
  (define-rule & () #i(turn (v 0 (- theta) 0)))
  (define-rule + () #i(turn (v 0 0 theta)))
  (define-rule - () #i(turn (v 0 0 (- theta)))))

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
