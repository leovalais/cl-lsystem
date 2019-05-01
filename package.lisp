(defpackage :cl-lsystem
  (:use :cl :gutils :iterate :alexandria)
  (:shadow
   eval ; cl
   save) ; gutils
  (:export process))
