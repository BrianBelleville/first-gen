(in-package :first-gen-internal)

(export '*get-req-handlers*)
(defvar *get-req-handlers* (make-hash-table :test #'equal))

(export '*post-req-handlers*)
(defvar *post-req-handlers* (make-hash-table :test #'equal))