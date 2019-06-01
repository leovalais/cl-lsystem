# cl-lsystem
[![CircleCI](https://circleci.com/gh/leovalais/cl-lsystem/tree/master.svg?style=svg)](https://circleci.com/gh/leovalais/cl-lsystem/tree/master)

A 2D/3D L-System rendering program and library written in Common Lisp.

## Example of definition

This is an adaptation of the L-System defined in figure 1.25 of
http://www.geekyblogger.com/2008/04/tree-and-l-system.html.

### Formal grammar

```
δ = 22.5°
ω = BA

A -> [&FLA]>>>>>[&FLA]>>>>>>>[&FLA]
F -> S>>>>>F
S -> FL
L -> [^^G{-J+J+J-!-J+J+J}$]
```

### `cl-lsystem`-compatible definition

```lisp
(define-material *branch-mtl* :obj
  :diffuse (v 0.51 0.32 0.0))
(define-material *leaf-mtl* :obj
  :diffuse (v 0.3 0.66 0.23))

(define-lsystem *figure-1.25* #wBA)

(define-rule A () #i(noop)
  #w[&FLA]>>>>>[&FLA]>>>>>>>[&FLA])
(define-rule F () #i(forward 3)
  #wS>>>>>F)
(define-rule S () #i(noop)
  #wFL)
(define-rule L () #i(noop)
  #w[^^G{-J+J+J-!-J+J+J}$])

(define-rule J () #i(jump 0.5))
(define-rule ! () #i(yaw pi)) ; turn around
(define-rule { () #i(begin-fill))
(define-rule } () #i(end-fill))
(define-rule [ () #i(stack))
(define-rule ] () #i(unstack))
(define-rule B () #i(apply-material *branch-mtl*))
(define-rule G () #i(apply-material *leaf-mtl*))
(define-rule $ () #i(pop-material))
(let ((delta (deg->rad 22.5)))
  (define-rule < () #i(roll delta))
  (define-rule > () #i(roll (- delta)))
  (define-rule ^ () #i(pitch delta))
  (define-rule & () #i(pitch (- delta)))
  (define-rule + () #i(yaw delta))
  (define-rule - () #i(yaw (- delta))))

(obj :n 5 :edges-per-branch 5 :branch-radius 0.6 :branch-decay 0.7)
```

## Using it with Docker

1. Clone the repository and `cd` inside it.
2. Run:
   ```shell
   sudo docker build -t cl-lsystem .
   ```
   It will build the system into a Docker container named `cl-lsystem`.
3. To render one of the example L-Systems, run:
   ```shell
   sudo docker run -v /home/vleo/work/ing/isim/cl-lsystem:/data \
                   -e SCRIPT=/data/examples/arrowhead.lisp  \
                   -e OUT='/data/triangle' \
                   -e N='8' -e WIDTH='1400' -e HEIGHT='1200' -e ORIGIN='(v -650 -550)' \
                   cl-lsystem
   ```
   You should see a file `triangle.png` within your `cl-lsystem` directory.

## Using it with Common Lisp

### Prerequisites

Most of the dependencies of `cl-lsystem` can be found on Quicklisp.
The following, however, need to be installed manually.

* `gutils`: https://github.com/leovalais/gutils
  * `array-operations`: https://github.com/bendudson/array-operations
  * `cl-netpbm`: https://github.com/sjl/cl-netpbm

Cloning the repositories inside a directory that ASDF can access
(e.g.: `~/common-lisp`) should be enough.

### Installation

1. Clone the repository in a place where ASDF can find it (for example, `~/common-lisp/`).
2. Load the system using `(asdf:load-system :cl-lsystem)`.
3. **TODO**

### Quickstart

**TODO**
