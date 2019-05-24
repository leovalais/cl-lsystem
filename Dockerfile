FROM lokedhs/sbcl-quicklisp

COPY . /cl-lsystem

RUN ln -s /cl-lsystem ~/common-lisp # setup ASDF central repository

WORKDIR /cl-lsystem

RUN apt-get update \
  && apt-get install -y git \
  && rm -rf /var/lib/apt/lists/*
RUN git clone https://github.com/bendudson/array-operations.git
RUN git clone https://github.com/leovalais/gutils.git
RUN git clone https://github.com/sjl/cl-netpbm.git
RUN sbcl --script .dockersetup.lisp gutils/gutils.asd cl-lsystem.asd

CMD sbcl --script .dockercmd.lisp
