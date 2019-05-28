(in-package :cl-lsystem)

(defmacro with-disjoint-outcomes (&body forms)
  (let ((sum (float (loop :for f :in forms :sum (first f)))))
    (with-gensyms (x)
      `(let ((,x (random ,sum)))
         (cond
           ,@(loop :for (p . body) :in forms
                   :summing p :into cumul
                   :collect `((<= ,x ,cumul) ,@body))
           (t (error "with-disjoint-outcomes: inconsistent probabilities")))))))

(defun deg->rad (x)
  (* x (/ pi 180)))
