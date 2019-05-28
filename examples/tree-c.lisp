;; trees in http://www.geekyblogger.com/2008/04/tree-and-l-system.html

(define-lsystem *tree-c* #wFA)
(define-3d-turtle (deg->rad 15) 5)
(define-rule A () #i(noop) #w^FB>>>B>>>>>B)
(define-rule B () #i(noop) #w^^F>>>>>>A)

(obj)
