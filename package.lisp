(defpackage :cl-lsystem
  (:use :cl :gutils :iterate :alexandria :cl-arrows)
  (:shadow
   eval ; cl
   position ; cl (turtle slot)
   space ; cl (turtle3D)
   function ; cl (rule)
   rotate ; alexandria (instruction)
   save) ; gutils
  (:export
   ;; utils.lisp
   with-disjoint-outcomes
   deg->rad

   ;; lsystem.lisp
   parametrized-letter
   parametrized-word
   rule name function instruction
   lsystem axiom rules
   set-rule
   iter-lsystem
   *lsystem*
   define-lsystem
   define-rule
   parametric-word
   instruction-constructor

   ;; instruction.lisp
   instruction
   singleton-instruction
   noop
   jump
   forward
   rotate
   roll
   pitch
   yaw
   stack
   unstack
   begin-fill
   end-fill
   apply-material
   pop-material
   lisp

   ;; env.lisp
   turtle position
   turtle2d direction
   turtle3d space
   head left up
   with-3d-turtle-space
   space-unit
   environment
   2d-environment
   3d-environment branch-radius branch-decay fill-stack
   save
   with-updated-turtle
   update-turtle
   png-environment
   eval-in-graphics-state
   face
   obj-environment vertices lines faces-root faces-stack edges-per-branch
   obj-face-tree make-obj-face-tree oft-group-name oft-material oft-faces oft-subgroups
   obj-material diffuse
   define-material
   add-vertice
   add-face
   add-subgroup
   pop-face-stack

   ;; eval.lisp

   ;; endpoints
   process))

(defpackage :cl-lsystem/scripting
  (:use :cl :cl-lsystem :gutils)
  (:shadowing-import-from :cl-lsystem function position space save))
