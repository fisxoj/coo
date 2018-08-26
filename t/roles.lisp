(defpackage coo-test.roles
  (:use #:cl
	#:prove
	#:alexandria
	#:coo.roles))

(in-package #:coo-test.roles)

(plan 1)

(subtest "name-symbol"
  (is (coo.roles::name-symbol 'coo.roles::name-symbol) "coo.roles::name-symbol"
      "names internal symbols.")

  (let ((coo.roles:*context-package* (find-package :coo.roles)))
    (is (coo.roles::name-symbol 'coo.roles::name-symbol) "name-symbol"
	"names symbols relative to *context-package*."))

  (is (coo.roles::name-symbol 'common-lisp:list) "common-lisp:list"
      "names external symbols."))

(finalize)
