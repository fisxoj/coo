(defsystem coo-test
  :defsystem-depends-on ("prove-asdf")
  :depends-on ("coo"
	       "prove")
  :pathname "t"
  :components ((:test-file "coo"))
  :perform (test-op (op c)
		    (declare (ignore c))
		    (funcall (read-from-string "prove:run")
			     (system-relative-pathname :coo-test #P"t/"))))
