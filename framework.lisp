(in-package :first-gen)

;not intended to be exported
(defvar *http-response-header*)

;export for now, maybe abstract this
(defvar *http-header*)

(defun process-query-string (query-string)
  "processes a uri query string into key value pairs that can be passed to a function as &key arguments"
  (declare (type string query-string))
  (mapcan (lambda (str)
	    (declare (type string str))
	    (multiple-value-bind (key val) (break-string #\= str)
	      (declare (type string key val))
	      (list (string-keyword key) val)))
	  (split-string "&;" query-string)))

(declaim (inline set-header-field))
(defun set-header-field (field value)
  (declare (type symbol field)
	   (type hash-table *http-response-header*))
  (setf (gethash field *http-response-header*) value))

(defmacro defhandler (handler-table uri lambda-list &rest body)
  (declare (type string uri))
  `(prog1 ,uri ;uri can safely be evaluated twice since it is declared to be a string, which is self evaluating
       (setf (gethash ,uri ,handler-table) #'(lambda ,lambda-list ,@body))))

(defmacro defget (uri lambda-list &body body)
  "define a function to be called to service a get request
binds *http-header* to the http header of the request, binds *http-response-header
to a hash table to serve as the header for the response.
All parameters in lambda-list are implicitly declared as &key, so other lambda list keywords are not allowed"
  (declare (type string uri))
  (with-gensyms (query-string-param header-param)
  `(prog1 ,uri ;uri can safely be evaluated twice since it is declared to be a string, which is self evaluating
     (setf (gethash ,uri *get-req-handlers*)
	   (lambda (,query-string-param ,header-param)
	     (declare (type string ,query-string-param)
		      (type hash-table ,header-param))
	     (let ((*http-response-header* (make-hash-table))
		   (*http-header* ,header-param))
	       (values 
		(apply
		 #'(lambda (&key ,@lambda-list &allow-other-keys)  ,@body)
		 (process-query-string ,query-string-param))
		*http-response-header*)))))))
	     

(defmacro defpost (uri lambda-list &body body)
  "define a function to be called to service a post request"
  `(defhandler *post-req-handlers* ,uri ,lambda-list ,@body))

(defmacro defview (name lambda-list &body body)
  "define a function that will output html as a string - functions defined by defget or defpost will ususally return a function defined with defview
binds *html-output* to a string stream within the scope of body, so that html generated by the html macros will be returned in the form of a string.
There is no restriction on lambda list"
  (let ((declare-present (eq (caar body) 'declare)))
    `(defun ,name ,lambda-list
       ,@(if declare-present
	     (list (car body)))
       (with-output-to-string (*html-output*)
	 ,@(if declare-present
	       (cdr body)
	       body)))))

