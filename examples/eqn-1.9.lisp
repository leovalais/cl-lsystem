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
