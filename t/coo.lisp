(defpackage coo-test
  (:use #:cl
	#:cl-arrows
	#:coo
	#:prove))

(in-package #:coo-test)


(defun every-line-same-length-p (str)
  (-<>> str
	(str:split #\Newline <> :omit-nulls t)
	(mapcar #'length)
	(alexandria:map-permutations (lambda (a) (= (car a) (cadr a))) <> :length 2)
	(every #'identity)))

(plan 1)

(subtest "make-title"
  (labels ((try (title &rest args)
	     (with-output-to-string (s)
	       (apply #'coo::make-title title s args))))
    (ok (every-line-same-length-p (try "Something"))
	"Title length matches marker length.")

    (is (char (try "Another Thing" :level 1) 0) #\-
	"Marker changes when :param:`level` changes.")

    (ok (str:ends-with? #.(write-to-string #\Newline :escape nil) (try "Some Titile"))
	"Title block ends with a newline.")))

(finalize)
