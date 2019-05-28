(define-lsystem *siertri* #wG)
(define-rule F () #i(forward 5) #wG+F+G)
(define-rule G () #i(forward 5) #wF-G-F)
(define-rule + () #i(rotate (/ pi 3)))
(define-rule - () #i(rotate (/ pi -3)))

(png :n 8 :width 1400 :height 1200 :origin (v -650 -550))
