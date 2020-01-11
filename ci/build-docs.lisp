#!/usr/local/bin/sbcl --script

(load #P"/quicklisp/setup.lisp")

(ql:quickload :coo)

(push (uiop:getcwd) asdf:*central-registry*)

(defpackage coo-user
  (:use :cl))

(in-package :coo-user)

(defun find-primary-asd-file ()
  (first
   (sort (uiop:directory* (make-pathname :name :wild :type "asd"))
         #'<
         :key (alexandria:compose #'length #'pathname-name))))

(let* ((asd-file (find-primary-asd-file))
       (system-name (pathname-name asd-file))
       (docs-path (if (uiop:getenvp "DOCS_PATH")
                      (uiop:getenv "DOCS_PATH")
                      #P"docs/")))

  (ql:quickload system-name)
  (coo:document-system system-name :base-path docs-path))
