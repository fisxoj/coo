(defpackage coo.util
  (:use #:cl #:alexandria #:cl-arrows)

  (:export #:make-url
	   #:make-anchor
           #:with-root
           #:root)

  (:documentation "Package for utilities that help multiple other coo packages.  Things like reliably generating urls."))

(in-package :coo.util)


(defvar *root* ""
  "Path to be used in the URL as a prefix.

For example, let's pretend we have base-path = \"docs\" and are generating
page docs/example/sub/package.html.

Then to link from that page to a function described at the docs/example/other-sub.html
we have to use *root* = \"../../\". And the link will be: ../../example/other-sub.html

To manipulate with this var, use with-root macro and root function.
")


(defmacro with-root ((new-root) &body body)
  `(let ((*root* ,new-root))
     ,@body))


(defun root ()
  *root*)


(defun make-url (stream package &optional symbol-name type)
  "Make a url for a thing of type :param:`type`, named :param:`symbol-name` located in package :param:`package`."

  (unless (typep package 'package)
    (setf package (find-package (string-upcase package))))

  (cond
    ((equal package (find-package :common-lisp))
     (hyperspec:lookup symbol-name))
    (t
     (format stream "~a~(~a~).html~@[#~*~a~]"
             (root)
             (package-name package)
             symbol-name
             (make-anchor nil symbol-name type)))))


(defun make-anchor (stream &optional symbol-name type)
  "Generate an anchor string for RST."

  (format stream "~@[~(~a~)-~a~]" type (quri:url-encode (string-downcase symbol-name))))
