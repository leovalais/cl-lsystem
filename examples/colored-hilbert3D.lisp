(define-material *red* :obj
  :diffuse (v 1.0 0.0 0.0))
(define-material *green* :obj
  :diffuse (v 0.0 1.0 0.0))
(define-material *blue* :obj
  :diffuse (v 0.0 0.0 1.0))
(define-material *yellow* :obj
  :diffuse (v 1.0 1.0 0.0))


(define-lsystem *3d-hilbert* #wA)

(define-3d-turtle (/ pi 2))
(define-rule ! () #i(yaw pi)) ; turn around

(define-rule A () #i(noop)
  #wRB-F+CFC+F-D&F^D-F+&&CFC+F+B>>$)
(define-rule B () #i(noop)
  #wGA&F^CFB^F^D^^-F-D^!F^B!FC^F^A>>$)
(define-rule C () #i(noop)
  #wK!D^!F^B-F+C^F^A&&FA&F^C+F+B^F^D>>$)
(define-rule D () #i(noop)
  #wY!CFB-F+B!FA&F^A&&FB-F+B!FC>>$)

(define-rule $ () #i(pop-material))
(define-rule R () #i(apply-material *red*))
(define-rule G () #i(apply-material *green*))
(define-rule K () #i(apply-material *blue*))
(define-rule Y () #i(apply-material *yellow*))

(obj :n 2)
