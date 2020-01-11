(defpackage coo-test.util
  (:use #:cl
	#:rove
	#:alexandria
	#:coo.util))

(in-package :coo-test.util)

(deftest test-make-url
  (ok (string= (make-url nil :alexandria "EMPTYP" :function) "alexandria.1.0.0.html#function__emptyp")
      "Formats a url for a symbol in a non-common-lisp package.")

  (ok (string= (make-url nil :cl "LIST" :function) "http://www.lispworks.com/reference/HyperSpec/Body/a_list.htm")
      "Formats a url for a symbol in the common-lisp package (linking to the hyperspec.")

  (ok (string= (make-url nil :alexandria) "alexandria.1.0.0.html")
      "Formats a url for a package."))
