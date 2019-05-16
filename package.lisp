(defpackage :cl-lsystem
  (:use :cl :gutils :iterate :alexandria :cl-arrows)
  (:shadow
   eval ; cl
   position ; cl (turtle slot)
   space ; cl (turtle3D)
   save) ; gutils
  (:export process))
