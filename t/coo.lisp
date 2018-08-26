(defpackage coo-test
  (:use #:cl
	#:coo
	#:prove))

(in-package #:coo-test)

(plan 1)

(subtest "make-title"
  (is (coo::make-title "Something")
      "=========
Something
========="
      "Title length matches marker length.")

  (is (char (coo::make-title "Another Thing" :level 1) 0) #\-
      "Marker changes when :param:`level` changes."))

(finalize)
