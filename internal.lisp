(in-package :first-gen-internal)

(defvar *get-req-handlers* (make-hash-table :test #'equal))
(defvar *post-req-handlers* (make-hash-table :test #'equal))