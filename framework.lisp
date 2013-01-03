(in-package :first-gen)

(defvar *get-req-handlers* (make-hash-table :test #'equal))
(defvar *post-req-handlers* (make-hash-table :test #'equal))

(defmacro defhandler (handler-table uri lambda-list &rest body)
  (declare (type string uri))
  `(prog1 ,uri ;uri can safely be evaluated twice since it is declared to be a string, which is self evaluating
       (setf (gethash ,uri ,handler-table) #'(lambda ,lambda-list ,@body))))

(export 'defget)
(defmacro defget (uri lambda-list &body body)
  `(defhandler *get-req-handlers* ,uri ,lambda-list ,@body))

(export 'defpost)
(defmacro defpost (uri lambda-list &body body)
  `(defhandler *post-req-handlers* ,uri ,lambda-list ,@body))

(export 'defview)
(defmacro defview (name lambda-list &body body)
  `(defun ,name ,lambda-list
     (with-output-to-string (*html-output*)
       ,@body)))

(declaim (inline execute-handler-method))
(defun execute-handler-method (table uri params)
  (declare (type string uri)
	   (type hash-table table))
  (multiple-value-bind (val found) (gethash uri table)
    (if found
	(values (apply val params) t)
	(values nil nil))))

(export 'execute-get-method)
(declaim (inline execute-get-method))
(defun execute-get-method (uri &rest params)
  (declare (type string uri))
  (execute-handler-method *get-req-handlers* uri params))

(export 'execute-post-method)
(declaim (inline execute-post-method))
(defun execute-post-method (uri &rest params)
  (declare (type string uri))
  (execute-handler-method *post-req-handlers* uri params))

(export 'clear-get-handlers)
(defun clear-get-handlers ()
  (setf *get-req-handlers* (make-hash-table :test #'equal))
  t)
  
(export 'clear-post-handlers)
(defun clear-post-handlers ()
  (setf *post-req-handlers* (make-hash-table :test #'equal)))
