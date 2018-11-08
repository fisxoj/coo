(defsystem coo-test
  :defsystem-depends-on ("prove-asdf")
  :depends-on ("coo"
	       "rove")
  :pathname "t"
  :components ((:file "coo")
               (:file "roles"))
  :perform (test-op (op c)
		    (declare (ignore c))
		    (funcall (read-from-string "rove:run") c)))
