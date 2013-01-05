(in-package :first-gen-server-hooks)

(declaim (inline execute-handler-method))
(defun execute-handler-method (table uri params)
  (declare (type string uri)
	   (type hash-table table))
  (multiple-value-bind (val found) (gethash uri table)
    (if found
	(values (apply val params) t)
	(values nil nil))))

(declaim (inline execute-get-method))
(defun execute-get-method (uri &rest params)
  (declare (type string uri))
  (execute-handler-method *get-req-handlers* uri params))

(declaim (inline execute-post-method))
(defun execute-post-method (uri &rest params)
  (declare (type string uri))
  (execute-handler-method *post-req-handlers* uri params))

(defun clear-get-handlers ()
  (setf *get-req-handlers* (make-hash-table :test #'equal))
  t)
  
(defun clear-post-handlers ()
  (setf *post-req-handlers* (make-hash-table :test #'equal)))
