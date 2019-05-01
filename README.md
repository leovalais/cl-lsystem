# cl-lsystem
A 2D/3D L-System rendering program and library written in Common Lisp. 

## Installation

1. Clone the repository in a place where ASDF can find it (for example, `~/common-lisp/`),
2. load the system using `(asdf:load-system :cl-lsystem)`,
3. enjoy!

## Dependencies

https://github.com/leovalais/cl-lsystem/blob/f56b1e43acd95efc8c8a2243b8cd05a0768e696d/cl-lsystem.asd#L11-L17

Most of the dependencies can be installed through *quicklisp*.
You can find the `:gutils` system at https://github.com/leovalais/gutils.

## Examples

### L-System definition: Sierpinski triangle (curve version)

```lisp
(defparameter *siertri*
  '(:axiom (Fr)
    :rules ((F1 (Fr + F1 + Fr))
            (Fr (F1 - Fr - F1)))
    :turtle ((Fr (forward 5))
             (F1 (forward 5))
             (+ (turn (v 0 0 (/ pi 3))))
             (- (turn (v 0 0 (- (/ pi 3))))))))
```

Then render that L-System up to the 7th iteration and save it to `out.png`:

```lisp
(cl-lsystem:process *siertri* 7)
```
