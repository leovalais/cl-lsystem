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

(obj :n 2)
