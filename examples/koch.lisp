;;;; Koch curve L-System
;;;; Definition:
;;;;     ω = F
;;;;     δ = 90°
;;;;     F -> F+F-F-F+F

;; define the L-System `*koch*' with axiom ω = F
(define-lsystem *koch* #wF)

;; define the rewritting rules of the L-System
(define-rule F () ; its name is F and it takes no parameters
    #i(forward 5) ; its tells the turtle to draw a 5px line while moving (#i stands for "instruction")
  #wF+F-F-F+F)    ; rewritting substitution word <=> F -> F+F-F-F+F

;; turtle instructions which are not rewritting rules (no substitution word given)
(define-rule + ()
    #i(rotate (/ pi 2)))
(define-rule - ()
    #i(rotate (/ pi -2)))

;; produces a PNG image
(png)
