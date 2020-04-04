(defsystem coo
  :author "Matt Novenstern"
  :mailto "fisxoj@gmail.com"
  :license "LLGPLv3+"
  :homepage "https://fisxoj.github.io/coo/"
  :depends-on ("asdf"
               "alexandria"
	       "cl-arrows"
               "pp-toml"
               "str"
               "docutils"
	       "docparser"
               "hyperspec"
               "djula"
               "quri"
               #+sbcl "sb-introspect")
  :pathname "src"
  :components ((:file "util")
               (:file "config")
	       (:file "roles")
	       (:file "introspection")
               (:file "plugin")
               (:module plugins
                :components ((:file "github")))
               (:file "coo"))
  :in-order-to ((test-op (test-op coo/test)))
  :source-control (:git "https://github.com/fisxoj/coo.git")
  :description "Coo is a documentation processor that allows you to write your documentation in |RST|.  If you fill in all your docstrings, it will look at all the documentation for packages and systems and format the external symbols for each page.

.. |RST| replace:: ReStructured Text

To generate documentation for your system, you should be able to run::

  CL-USER> (coo:document-system :my-system)

and coo will take care of everything for you.  You can specify what directory it writes the documentation to as well (see :function:`coo:document-system`).  Coo is built on top of `cl-docutils`_ by John A.R. Williams, which is, in turn modeled on the Python implementation of `Docutils`_.

If you're interested in modifying the output of the documentation generation, you can look at the templates under the `templates/` directory in coo's source code.  Eventually, there will be an extensible `way of doing that <https://github.com/fisxoj/coo/issues/14>`_.

.. _cl-docutils: http://www.jarw.org.uk/lisp/cl-docutils.html
.. _Docutils: http://docutils.sourceforge.net/

The source of coo is available at `https://github.com/fisxoj/coo <https://github.com/fisxoj/coo>`_.")


(defsystem coo/test
  :depends-on ("coo"
	       "rove")
  :pathname "t"
  :components ((:file "coo")
               (:file "roles")
               (:file "util"))
  :perform (test-op (op c)
		    (declare (ignore c))
		    (funcall (read-from-string "rove:run") c)))
