(defpackage coo.plugins.github
  (:use #:cl #:alexandria #:cl-arrows))

(in-package :coo.plugins.github)

(defun definition-url (system object)
  (multiple-value-bind (relative-path line-number)
      (coo.introspection:find-definition-line-number system object)
    (format nil "`[source] <https://github.com/~a/blob/master/~a#L~d>`_" "fisxoj/coo" relative-path line-number)))
