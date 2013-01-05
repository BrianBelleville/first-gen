(declaim (optimize (speed 3)))

(defpackage :first-gen-internal
  (:documentation "internal symbols that must be shared between the package :first-gen and :first-gen-server-hooks")
  (:use :cl)
  (:export

   ;internal symbols
   :*get-req-handlers*
   :*post-req-handlers*))

(defpackage :first-gen
  (:documentation "the macros for generating html and creating a dynamic website using the :first-gen framework")
  (:use :cl
	:bel-utils
	:first-gen-internal)
  (:export

   ; framework symbols
   :defget
   :defpost
   :defview

   ; html-gen symbols
   :*html-out

   ; html-tags symbols
   :doctype-html
   :area
   :base
   :br
   :col
   :command
   :embed
   :hr
   :img
   :input
   :keygen
   :link
   :meta
   :param
   :source
   :track
   :wbr 
   :a
   :abbr
   :acronym
   :address
   :b
   :bdo
   :big
   :blockquote
   :body
   :button
   :caption
   :cite
   :code
   :colgroup
   :dd
   :del
   :dfn
   :div
   :dl
   :dt
   :em
   :fieldset
   :form
   :h1
   :h2
   :h3
   :h4
   :h5
   :h6
   :head
   :html
   :i
   :ins
   :kbd
   :label
   :legend
   :li
   :html-map
   :noscript
   :object
   :ol
   :optgroup
   :option
   :p
   :pre
   :q
   :samp
   :script
   :select
   :small
   :span
   :strong
   :style
   :sub
   :sup
   :table
   :tbody
   :td
   :textarea
   :tfoot
   :th
   :thead
   :title
   :tr
   :tt
   :ul
   :html-var))

(defpackage :first-gen-server-hooks
  (:documentation "symbols that are needed so set up a server to serve sites defined with the :first-gen framework")
  (:use :cl
	:first-gen-internal)
  (:export

   ;server-hooks symbols
   :execute-get-method
   :execute-post-method
   :clear-get-handlers
   :clear-post-handlers))