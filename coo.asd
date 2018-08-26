(defsystem coo
  :author "Matt Novenstern <fisxoj@gmail.com>"
  :license "LLGPLv3+"
  :depends-on ("alexandria"
	       "cl-arrows"
               "docutils")
  :pathname "src"
  :components ((:file "roles")
               (:file "coo"))
  :in-order-to ((test-op (test-op coo-test))))
