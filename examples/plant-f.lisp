;; plant in http://algorithmicbotany.org/papers/abop/abop-ch1.pdf figure 1.24

(define-lsystem *plant-f* #wX)
(define-rule X () #i(forward 5) #wF-[[X]+X]+F[+FX]-X)
(define-rule F () #i(forward 5) #wFF)
(define-rule + () #i(rotate (deg->rad 22.5)))
(define-rule - () #i(rotate (deg->rad 22.5)))
(define-rule [ () #i(stack))
(define-rule ] () #i(unstack))

(png :direction (v 0.0 1.0))
