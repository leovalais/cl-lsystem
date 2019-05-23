(defpackage :cl-lsystem
  (:use :cl :gutils :iterate :alexandria :cl-arrows)
  (:shadow
   eval ; cl
   position ; cl (turtle slot)
   space ; cl (turtle3D)
   function ; cl (rule)
   rotate ; alexandria (instruction)
   save) ; gutils
  (:export process))
