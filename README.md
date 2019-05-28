# cl-lsystem
A 2D/3D L-System rendering program and library written in Common Lisp.

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
                   -e OUT='/data/triangle' -e N='8' -e WIDTH='1400' -e HEIGHT='1200' -e ORIGIN='(v -650 -550)' \
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
