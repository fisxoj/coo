(defpackage coo.util
  (:use #:cl #:alexandria #:cl-arrows)

  (:export #:make-url
	   #:make-anchor)

  (:documentation "Package for utilities that help multiple other coo packages.  Things like reliably generating urls."))

(in-package :coo.util)


(defun make-url (stream package &optional symbol-name type)
  "Make a url for a thing of type :param:`type`, named :param:`symbol-name` located in package :param:`package`."

  (unless (typep package 'package)
    (setf package (find-package (string-upcase package))))

  (cond
    ((equal package (find-package :common-lisp))
     (hyperspec:lookup symbol-name))
    (t
     (format stream "~(~a~).html~@[#~*~a~]"
             (package-name package)
             symbol-name
             (make-anchor nil symbol-name type)))))


(defun make-anchor (stream &optional symbol-name type)
  "Generate an anchor string for RST."

  (format stream "~@[~(~a~)-~a~]" type (quri:url-encode (string-downcase symbol-name))))
