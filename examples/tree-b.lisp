;; trees in http://www.geekyblogger.com/2008/04/tree-and-l-system.html

(define-lsystem *tree-b* #wFA)
(define-3d-turtle (deg->rad 30) 5)
(define-rule A () #i(noop) #wF[^BL]>>[^BL]>>A)
(define-rule B () #i(noop) #wF[-BL]B)

(obj)
