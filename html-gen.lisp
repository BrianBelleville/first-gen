(in-package :first-gen)

(defvar *html-output* *standard-output*)

(declaim (inline output-html))
(defgeneric output-html (object)
  (:documentation "a generic method to control how different objects should be output to the html-stream"))

(defmethod output-html ((object string))
  (write-sequence object *html-output*))

(defmethod output-html (object)
  (princ object *html-output*))

(defun transform-attribute-list (lst)
  "takes a list of parameters expressed as key arguments, transforms the keys into strings, and leaves the values unchanged"
  (do* ((list lst (cddr list))
	(key (car list) (car list))
	(val (cadr list) (cadr list))
	(acc nil))
       ((null list) (nreverse acc))
    (push (string-downcase (symbol-name key)) acc)
    (push val acc)))

(defun gen-body-clause (form)
  "takes a form from a body in an html tag, and creates the code to output the item based on information known at compile time"
  (cond ((typep form 'string) `(write-sequence ,form *html-output*))
	((typep form 'number) `(princ ,form *html-output*))
	((typep form 'character) `(write-char ,form *html-output*))
	((typep form 'null) nil)
	(t ; default: determine at runtime
	 (with-gensyms (formsym)
	   `(aifn (,formsym ,form)
		  (output-html ,formsym))))))

(defun compile-body (body)
  "compiles the forms in body of an html tag to be output to the html stream"
  (remove-if #'null
	     (mapcar #'gen-body-clause
			       body)))

(defun tag-name (symb)
  "since some html tags use symbols that are defined in the common-lisp package, the macros for some tag names will be prefaced by 'html-', this will strip that off of the html that will be generated and output."
  (let ((name (string-downcase (symbol-name symb))))
    (if (and (>= (length name) 5) (equal (subseq name 0 5) "html-"))
	(subseq name 5)
	name)))

(defun make-html-tag-strings (symb &optional void-tag)
  "creates strings to be used as literals in the html-tag macros"
  (let* ((tag-name (tag-name symb))
	 (open-with-attributes (format nil "<~A" tag-name))
	 (open-no-attributes (format nil"<~A>" tag-name))
	 (closing (if void-tag
		      nil
		      (format nil "</~A>" tag-name)))
	 (open+closing (if void-tag
			   (format nil "<~A />" tag-name)
			   (concatenate 'string open-no-attributes closing))))
    (values
     open-with-attributes
     open-no-attributes
     closing
     open+closing)))

(defmacro deftag (name &optional void-tag)
  (multiple-value-bind (open-with-attributes open-no-attributes closing open+closing) (make-html-tag-strings name void-tag)
    (if void-tag
	(let ((attribute-format-string (concatenate 'string open-with-attributes "~{ ~A=\"~A\"~} />")))
	  `(defmacro ,name (&optional attributes)
	     `(progn
		,(if attributes
		    `(format *html-output*
			     ,,attribute-format-string
			     (list ,@(transform-attribute-list attributes)))
		    `(write-sequence ,,open+closing *html-output*))
		,nil)))
	(let ((attribute-format-string (concatenate 'string open-with-attributes "~{ ~A=\"~A\"~}>"))) 
	  `(defmacro ,name (attributes &body body)
	     `(progn
					; this is conditional on whether attributes or body are null, and there are 4 cases
	      ,@(cond ((or
			(and attributes body)
			(and attributes (not body)))
					;handle the cases where attributes is present
		       `((format *html-output*
				 ,,attribute-format-string
				 (list ,@(transform-attribute-list attributes)))
			 ,@(compile-body body)
			 (write-sequence ,,closing *html-output*)))
		      ((and (not attributes) body) 
		       `((write-sequence ,,open-no-attributes *html-output*)
			 ,@(compile-body body)
			 (write-sequence ,,closing *html-output*)))
		      ((and (not body) (not attributes))
		       `((write-sequence ,,open+closing *html-output*))))
	      ,nil))))))

(defmacro deftags  (export-tags &rest tag-specs)
  "defines multiple tags at once, if export-tags, all tags will be exported"
  (let* ((tag-specs (mapcar #'(lambda (tag-spec)
			       (if (atom tag-spec)
				   (list tag-spec)
				   tag-spec))
			    tag-specs))
	 (tag-names (mapcar #'(lambda (tag-spec)
				(destructuring-bind (tag-name &optional void-tag) tag-spec
				  (declare (ignore void-tag))
				  tag-name))
			    tag-specs)))
    `(progn
       ,@(if export-tags
	     (list `(export ',tag-names)))
       ,@(mapcar #'(lambda (tag-spec)
		     `(deftag ,@tag-spec))
		 tag-specs))))

