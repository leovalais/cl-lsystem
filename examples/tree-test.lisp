(define-lsystem *tree-test* #wA)
(define-3d-turtle (/ pi 3) 3)
(define-rule A () #i(noop) #wF[&B][^B][+B][-B])
(define-rule B () #i(noop) #wFA)

(obj)
