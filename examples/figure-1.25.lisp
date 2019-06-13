;; figure in http://www.geekyblogger.com/2008/04/tree-and-l-system.html

(define-material *branch-mtl* :obj
  :diffuse (v 0.63 0.32 0.18))
(define-material *leaf-mtl* :obj
  :diffuse (v 0.3 0.66 0.23))

(define-lsystem *figure-1.25* #wBA)
(define-rule A () #i(noop)
  #w[&FLA]>>>>>[&FLA]>>>>>>>[&FLA])
(define-rule F () #i(forward 3)
  #wS>>>>>F)
(define-rule S () #i(noop)
  #wFL)
(define-rule L () #i(noop)
  #w[^^G{-J+J+J-!-J+J+J}$])
(define-rule J () #i(jump 0.5))
(define-rule ! () #i(yaw pi)) ; turn around
(define-rule { () #i(begin-fill))
(define-rule } () #i(end-fill))
(define-rule [ () #i(stack))
(define-rule ] () #i(unstack))
(define-rule B () #i(apply-material *branch-mtl*))
(define-rule G () #i(apply-material *leaf-mtl*))
(define-rule $ () #i(pop-material))
(let ((delta (deg->rad 22.5)))
  (define-rule < () #i(roll delta))
  (define-rule > () #i(roll (- delta)))
  (define-rule ^ () #i(pitch delta))
  (define-rule & () #i(pitch (- delta)))
  (define-rule + () #i(yaw delta))
  (define-rule - () #i(yaw (- delta))))

(obj :n 5 :edges-per-branch 5 :branch-radius 0.6 :branch-decay 0.7)
