(defpackage coo-test
  (:use #:cl
	#:arrows
	#:coo
	#:rove))

(in-package #:coo-test)


(defun every-line-same-length-p (str)
  (-<>> str
	(str:split #\Newline <> :omit-nulls t)
	(mapcar #'length)
	(alexandria:map-permutations (lambda (a) (= (car a) (cadr a))) <> :length 2)
	(every #'identity)))

(deftest test-make-title
  (labels ((try (title &rest args)
	     (with-output-to-string (s)
	       (apply #'coo::make-title title s args))))
    (ok (every-line-same-length-p (try "Something"))
	"Title length matches marker length.")

    (ok (char= (char (try "Another Thing" :level 1) 0) #\-)
	"Marker changes when :param:`level` changes.")

    (ok (str:ends-with? #.(write-to-string #\Newline :escape nil) (try "Some Titile"))
	"Title block ends with a newline.")))

(deftest test-remove-unintentional-whitespace
    (let ((cases '("It leaves an empty string alone." ("" "")
                   "It strips leading single spaces from docstrings." ("First line
 second line." "First line
second line.")
                   "It doesn't strip leading whitespace of two or more spaces." ("Here's an example
::
  cl-user> (string= \"potato\" \"yam\")
  nil" "Here's an example
::
  cl-user> (string= \"potato\" \"yam\")
  nil"))))
      (loop for (description (input expected)) on cases by #'cddr
            do (ok (string= (djula.filters:remove-unintentional-whitespace input) expected)
                   description))))
