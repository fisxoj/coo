(defsystem coo
  :author "Matt Novenstern"
  :mailto "fisxoj@gmail.com"
  :license "LLGPLv3+"
  :depends-on ("asdf"
               "alexandria"
	       "cl-arrows"
               "str"
               "docutils"
               "quri")
  :pathname "src"
  :components ((:file "roles")
               (:file "coo"))
  :in-order-to ((test-op (test-op coo-test)))
  :description "Coo is a documentation processor that allows you to write your documentation in |RST|.  If you fill in all your docstrings, it will look at all the documentation for packages and systems and format the external symbols for each page.

.. |RST| replace:: ReStructured Text

To generate documentation for your system, you should be able to run::
  CL-USER> (coo:document-system :my-system)

and coo will take care of everything for you.  You can specify what directory it writes the documentation to as well.  Coo is built on top of `cl-docutils`_ by John A.R. Williams, which is, in turn modeled on the Python implementation of `Docutils`_.

.. _cl-docutils: http://www.jarw.org.uk/lisp/cl-docutils.html
.. _Docutils: http://docutils.sourceforge.net/")
