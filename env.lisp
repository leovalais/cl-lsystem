(in-package :cl-lsystem)

(defstruct turtle
  (position (v0 3) :type vect)
  (orientation (v0 3) :type vect))

(defclass environment ()
  ((turtle :initform (make-turtle)
           :accessor turtle)
   (stack :initform ()
          :accessor env-stack
          :type list)))

;; env initialization in a initialize-instance :after method

(defgeneric save (env filename))
(defgeneric stack (env)
  (:method-combination progn :most-specific-last))
(defgeneric unstack (env)
  (:method-combination progn :most-specific-first))

(defgeneric update-turtle (env &key position orientation)
  (:method ((env environment) &key position orientation)
    (let ((turtle (turtle env)))
      (when position
        (setf (turtle-position turtle) position))
      (when orientation
        (setf (turtle-orientation turtle) orientation)))))

(defmethod stack progn ((env environment))
  (push (turtle env) (env-stack env)))

(defmethod unstack progn ((env environment))
  (setf (turtle env) (pop (env-stack env))))

;; (vecto:with-canvas (:width 100 :height 100)
;;   X)

(defclass png-environment (environment)
  ((vecto-graphics-state :accessor vecto-graphics-state)
   (width :initform 2000
          :initarg :width)
   (height :initform 2000
           :initarg :height)
   (origin :initform (v 0 0)
           :initarg :origin
           :type V2)))

(defclass obj-environment (environment)
  ()) ; FIXME tbd


(defmethod initialize-instance :after ((env png-environment) &key)
  (setf (vecto-graphics-state env)
        (make-instance 'vecto::graphics-state))
  (with-slots (width height origin) env
    (vecto::state-image (vecto-graphics-state env) width height)
    (let* ((translation (v- (v (/ width 2)
                               (/ height 2))
                            origin))
           (-translation (v- translation)))
      (eval-in-graphics-state env (lambda ()
                                    (vecto:set-rgb-fill 1.0 1.0 1.0)
                                    (vecto:set-rgb-stroke 0.0 0.0 0.0)
                                    (vecto:set-line-width 1)
                                    (vecto:translate (vx translation) (vy translation))
                                    (vecto:rectangle (vx -translation) (vy -translation) width height)
                                    (vecto:move-to 0 0))))))

(defgeneric eval-in-graphics-state (env fun)
  (:method ((env png-environment) fun)
    (let ((vecto::*graphics-state* (vecto-graphics-state env)))
      (funcall fun))))

(defmethod save ((env png-environment) filename)
  (let ((vecto::*graphics-state* (vecto-graphics-state env)))
    (vecto:fill-and-stroke)
    (vecto:save-png (format nil "~a.png" filename))
    (vecto::clear-state vecto::*graphics-state*)))
