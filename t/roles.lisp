(defpackage coo-test.roles
  (:use #:cl
	#:prove
	#:alexandria
	#:coo.roles))

(in-package #:coo-test.roles)

(plan 2)

(subtest "name-symbol"
  (is (name-symbol 'coo.roles::defref) "coo.roles::defref"
      "names internal symbols.")

  (let ((*context-package* (find-package :coo.roles)))
    (is (name-symbol 'name-symbol) "name-symbol"
	"names symbols relative to *context-package*."))

  (is (name-symbol 'common-lisp:list) "common-lisp:list"
      "names external symbols."))

(subtest "find-symbol-by-name"
  (ok (null (find-symbol-by-name "unknown-symbol"))
      "Returns nil for an unknown symbol.")

  (is (find-symbol-by-name "cl:list") 'list
      "finds symbols in the cl package.")

  (is (find-symbol-by-name "coo.roles::defref") 'coo.roles::defref
      "finds internal symbols in a package.")

  (is (find-symbol-by-name "coo.roles:find-symbol-by-name") 'find-symbol-by-name
      "finds external symbols in a named package.")

  (let ((*context-package* (find-package :coo.roles)))
    (is (find-symbol-by-name "find-symbol-by-name") 'find-symbol-by-name
	"finds external symbols in a *context-package*.")))

(finalize)
