(defpackage coo.roles
  (:use #:cl
        #:alexandria
        #:cl-arrows
        #:docutils.parser.rst)
  (:export #:*context-package*))

(in-package :coo.roles)

(defvar *context-package* nil
  "If non-nil, the current package context.")

(defun name-symbol (symbol)
  "Used for formatting names of symbols.  Will take into account :variable:`*context-package* if non-nil.

For example::
  coo.roles> (name-symbol 'name-symbol)
  \"coo.roles::name-symbol\"

  coo.roles> (let ((*context-package* (find-package 'coo.roles)))
               (name-symbol 'name-symbol))
  \"name-symbol\"

  coo.roles> (name-symbol 'cl:list)
  \"common-lisp:list\"
"

  (-> (if (equal *context-package* (symbol-package symbol))
          (symbol-name symbol)
          (multiple-value-bind (symbol status)
              (find-symbol (symbol-name symbol) (symbol-package symbol))
            (concatenate 'string
                         (-> symbol symbol-package package-name)
                         (if (eq :internal status) "::" ":")
                         (symbol-name symbol))))
      string-dopwncase))

(defmacro defref (thing)
  `(def-role ,thing (symbol)
     (if-let ((found-symbol (read-from-string symbol)))
       (let ((node (docutils:make-node 'docutils.nodes:reference
                                       :refuri (string-downcase
                                                (format nil ,(concatenate 'string
                                                                          "~a.html#"
                                                                          (string-downcase thing)
                                                                          "__~a")
                                                        (-> found-symbol
                                                            symbol-package
                                                            package-name)
                                                        (symbol-name found-symbol)))
                                       :class ,(concatenate 'string "ref-" (string-downcase thing)))))

         (docutils:add-child node (name-symbol found-symbol))
         node)
       (docutils:report :error
                        `("Wasn't able to find ~a, make sure it's spelled correctly and defined!" ,symbol)))))

(defref function)

(defref class)

(defref macro)

(defref variable)

(def-role package (name)
  (if-let ((package (uiop:find-package* (string-upcase name) nil)))
    (let ((node (docutils:make-node 'docutils.nodes:reference
                                    :refuri (format nil "~a.html"
                                                    (-> package
                                                        package-name
                                                        string-downcase))
                                    :class "ref-package")))

      (docutils:add-child node (-> package
                                   package-name
                                   string-downcase))
      node)
    (docutils:report :error
                     `("Wasn't able to find package ~a, make sure it's spelled correctly and defined!" ,name))))
