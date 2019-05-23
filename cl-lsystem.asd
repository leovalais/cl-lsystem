;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(asdf:defsystem :cl-lsystem
  :name "cl-lsystem"
  :version "0.0.0"
  :maintainer "Léo Valais"
  :author "Léo Valais"
  :licence "MIT"
  :description "I am Groot"

  :depends-on (:iterate
                :alexandria
                :rte
                :gutils
                :closer-mop
                :vecto
                :cl-arrows)

  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "lsystem")
               (:file "instruction")
               (:file "env")
               (:file "eval")
               (:file "endpoints")
               (:file "examples")))
