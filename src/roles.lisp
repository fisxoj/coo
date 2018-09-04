(defpackage coo.roles
  (:use #:cl
        #:alexandria
        #:cl-arrows
        #:docutils.parser.rst)
  (:export #:*context-package*
	   #:name-symbol
           #:anchor-name-for-symbol
	   #:find-symbol-by-name)
  (:documentation "This package defines a few new roles that are helpful for lisp documentation."))

(in-package :coo.roles)

(defvar *context-package* nil
  "If non-nil, the current package context.  Influences the reading and printing of symbols by :function:`name-symbol` and :function:`find-symbol-by-name`.")


(defun anchor-name-for-symbol (symbol)
  "Generates a url-safe name for :param:`symbol`.

e.g. *some-variable* => \"%2Asome-variable%2A\""

  (-> symbol
      symbol-name
      quri:url-encode))


(defun name-symbol (symbol)
  "Used for formatting names of symbols.  Will take into account :variable:`*context-package*` if non-nil.

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
      string-downcase))


(defun find-symbol-by-name (str)
  "Search for the symbol named by :param:`str` in the current context.  If it's unqualified, look in :variable:`*context-package*`, then ``common-lisp`` then give up."

  (declare (type string str))

  (let* ((qualified-p (position #\: str :test #'char=))
	 (internal-p (str:containsp "::" str))
	 (symbol-name (string-upcase (subseq str (if qualified-p (+ qualified-p (if internal-p 2 1)) 0)))))

    (if qualified-p
	(when-let ((package (find-package (string-upcase (subseq str 0 qualified-p)))))
	  (find-symbol symbol-name package))
	(if *context-package*
	    (find-symbol symbol-name *context-package*)
	    (find-symbol symbol-name (find-package :common-lisp))))))


(defmacro defref (thing)
  `(def-role ,thing (symbol)
     (if-let ((found-symbol (find-symbol-by-name symbol)))
       (let ((node (docutils:make-node 'docutils.nodes:reference
                                       :refuri (string-downcase
                                                (format nil ,(concatenate 'string
                                                                          "~a.html#"
                                                                          (string-downcase thing)
                                                                          "-~a")
                                                        (-> found-symbol
                                                            symbol-package
                                                            package-name)
                                                        (anchor-name-for-symbol found-symbol)))
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
