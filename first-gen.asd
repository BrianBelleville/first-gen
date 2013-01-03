(asdf:defsystem :first-gen
  :description "Html generating macros"
  :author "Brian Belleville"
  :depends-on (:bel-utils)
  :serial t
  :components ((:file "package")
	       (:file "html-gen")
	       (:file "html-tags")
	       (:file "framework")
	       (:file "server-hooks")))