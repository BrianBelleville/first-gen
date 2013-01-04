(in-package :first-gen)

(defmacro defhandler (handler-table uri lambda-list &rest body)
  (declare (type string uri))
  `(prog1 ,uri ;uri can safely be evaluated twice since it is declared to be a string, which is self evaluating
       (setf (gethash ,uri ,handler-table) #'(lambda ,lambda-list ,@body))))

(export 'defget)
(defmacro defget (uri lambda-list &body body)
  "define a function to be called to service a get request"
  `(defhandler *get-req-handlers* ,uri ,lambda-list ,@body))

(export 'defpost)
(defmacro defpost (uri lambda-list &body body)
  "define a function to be called to service a post request"
  `(defhandler *post-req-handlers* ,uri ,lambda-list ,@body))

(export 'defview)
(defmacro defview (name lambda-list &body body)
  "define a function that will output html as a string - functions defined by defget or defpost will ususally return a function defined with defview"
  (let ((declare-present (eq (caar body) 'declare)))
    `(defun ,name ,lambda-list
       ,@(if declare-present
	     (list (car body)))
       (with-output-to-string (*html-output*)
	 ,@(if declare-present
	       (cdr body)
	       body)))))

