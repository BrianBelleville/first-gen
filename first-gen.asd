(asdf:defsystem :first-gen
  :description "Html generating macros"
  :author "Brian Belleville"
  :depends-on (:bel-utils)
  :components ((:file "package")
	       (:file "internal" :depends-on ("package"))
	       (:file "html-gen" :depends-on ("package"))
	       (:file "html-tags" :depends-on ("package" "html-gen"))
	       (:file "framework" :depends-on ("package" "internal" "html-gen"))
	       (:file "server-hooks" :depends-on ("package" "internal"))))