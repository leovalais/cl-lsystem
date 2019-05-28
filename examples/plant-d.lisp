;; plant in http://algorithmicbotany.org/papers/abop/abop-ch1.pdf figure 1.24

(define-lsystem *plant-d* #wX)
(define-rule X () #i(forward 5) #wF[+X]F[-X]+X)
(define-rule F () #i(forward 5) #wFF)
(define-rule + () #i(rotate (/ pi 9)))
(define-rule - () #i(rotate (/ pi -9)))
(define-rule [ () #i(stack))
(define-rule ] () #i(unstack))

(png :direction (v 0.0 1.0))
