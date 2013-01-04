(declaim (optimize (speed 3)))

(defpackage :first-gen-internal
  (:documentation "internal symbols that must be shared between the package :first-gen and :first-gen-server-hooks")
  (:use :cl
	:bel-utils))

(defpackage :first-gen
  (:documentation "the macros for generating html and creating a dynamic website using the :first-gen framework")
  (:use :cl
	:bel-utils
	:first-gen-internal))

(defpackage :first-gen-server-hooks
  (:documentation "symbols that are needed so set up a server to serve sites defined with the :first-gen framework")
  (:use :cl
	:first-gen-internal))