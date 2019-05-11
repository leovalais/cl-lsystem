(defpackage :cl-lsystem
  (:use :cl :gutils :iterate :alexandria :cl-arrows)
  (:shadow
   eval ; cl
   position ; cl (turtle slot)
   save) ; gutils
  (:export process))
