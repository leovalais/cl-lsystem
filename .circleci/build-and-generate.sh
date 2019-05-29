#!/bin/sh
set -ex

ln -s `pwd` ~/common-lisp
apt-get update && apt-get install -y git && rm -rf /var/lib/apt/lists/*
(git clone https://github.com/bendudson/array-operations.git &
 git clone https://github.com/leovalais/gutils.git &
 git clone https://github.com/sjl/cl-netpbm.git)
sbcl --script .dockersetup.lisp gutils/gutils.asd cl-lsystem.asd

mkdir results
for script in examples/*.lisp; do
    out=$(basename $script)
    out="results/${out%.*}"
    SCRIPT=$script OUT=$out sbcl --script .dockercmd.lisp
done
