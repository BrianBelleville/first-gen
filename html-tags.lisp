(in-package :first-gen)

(export 'doctype-html)
(declaim (inline doctype-html))
(defun doctype-html ()
  "outputs the doctype declaration to *html-output*"
  (write-sequence "<!DOCTYPE html>" *html-output*))

(deftags t
    (area t)
  (base t)
  (br t)
  (col t)
  (command t)
  (embed t)
  (hr t)
  (img t)
  (input t)
  (keygen t)
  (link t)
  (meta t)
  (param t)
  (source t)
  (track t)
  (wbr t)
  a
  abbr
  acronym
  address
  b
  bdo
  big
  blockquote
  body
  button
  caption
  cite
  code
  colgroup
  dd
  del
  dfn
  div
  dl
  dt
  em
  fieldset
  form
  h1
  h2
  h3
  h4
  h5
  h6
  head
  html
  i
  ins
  kbd
  label
  legend
  li
  html-map
  noscript
  object
  ol
  optgroup
  option
  p
  pre
  q
  samp
  script
  select
  small
  span
  strong
  style
  sub
  sup
  table
  tbody
  td
  textarea
  tfoot
  th
  thead
  title
  tr
  tt
  ul
  html-var)
