;this is the hooks that are necessary for a server to support the first-gen framework.
; todo : these symbols don't need to be made available to the general framework, so it may be better to define the shared symbols used by the functions exported here in some private package that both the server hooks and the main framework utilize.

(defpackage :first-gen-server-hooks
  (:use :cl :first-gen)
  (:export :execute-get-method
	   :execute-post-method
	   :clear-get-handlers
	   :clear-post-handlers))