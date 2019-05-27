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
  :defsystem-depends-on (:rte)

  :serial t
  :components ((:file "package")
               (:file "utils")
               (:rte-file "lsystem")
               (:file "instruction")
               (:rte-file "env")
               (:file "eval")
               (:file "endpoints")
               (:file "examples")))
