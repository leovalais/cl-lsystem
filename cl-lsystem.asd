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
                :array-operations
                :closer-mop
                :vecto)

  :serial t
  :components ((:file "package")
               (:file "definitions")
               (:file "parse")
               (:file "grammar")
               (:file "instruction")
               (:file "env")
               (:file "eval")
               (:file "lsystem")
               (:file "examples")))
