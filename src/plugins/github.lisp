(defpackage coo.plugins.github
  (:use #:cl))

(in-package :coo.plugins.github)

(defun metadata (stream system object)
  (multiple-value-bind (relative-path line-number)
      (coo.introspection:find-definition-line-number system object)
    (when (not (null relative-path))
      (format stream "~&`\[source\] <https://github.com/~a/blob/master/~a~@[#L~d~]>`_~%"
              (coo.plugin:config "project")
              relative-path
              line-number))))
