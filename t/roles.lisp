(defpackage coo-test.roles
  (:use #:cl
	#:rove
	#:alexandria
	#:coo.roles))

(in-package #:coo-test.roles)

(deftest test-name-symbol
  (ok (string= (name-symbol 'coo.roles::defref) "coo.roles::defref")
      "names internal symbols.")

  (let ((*context-package* (find-package :coo.roles)))
    (ok (string= (name-symbol 'name-symbol) "name-symbol")
	"names symbols relative to *context-package*."))

  (ok (string= (name-symbol 'common-lisp:list) "common-lisp:list")
      "names external symbols."))

(deftest test-find-symbol-by-name
  (ok (null (find-symbol-by-name "unknown-symbol"))
      "Returns nil for an unknown symbol.")

  (ok (eq (find-symbol-by-name "cl:list") 'list)
      "finds symbols in the cl package.")

  (ok (eq (find-symbol-by-name "coo.roles::defref") 'coo.roles::defref)
      "finds internal symbols in a package.")

  (ok (eq (find-symbol-by-name "coo.roles:find-symbol-by-name") 'find-symbol-by-name)
      "finds external symbols in a named package.")

  (let ((*context-package* (find-package :coo.roles)))
    (ok (eq (find-symbol-by-name "find-symbol-by-name") 'find-symbol-by-name)
	"finds external symbols in a *context-package*.")))
