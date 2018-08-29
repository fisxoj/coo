(defsystem coo
  :author "Matt Novenstern <fisxoj@gmail.com>"
  :license "LLGPLv3+"
  :depends-on ("asdf"
               "alexandria"
	       "cl-arrows"
               "str"
               "docutils")
  :pathname "src"
  :components ((:file "roles")
               (:file "coo"))
  :in-order-to ((test-op (test-op coo-test))))
