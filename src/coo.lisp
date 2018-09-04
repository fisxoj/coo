(defpackage coo
  (:use #:cl
        #:cl-arrows
        #:alexandria)
  (:import-from #:docutils
                #:add-child)
  (:export #:document-package
           #:document-system)
  (:documentation "Generally, you'll want to use :function:`document-system` to start yourself off.

.. try running it like this::

    CL-USER> (ql:quickload :coo)
    CL-USER> (coo:document-system :my-cool-system)
"))

(in-package :coo)

(defparameter +headings+ '(#\= #\- #\_ #\~ #\^))


(defun package-path (package)
  "Provide a pathname for a package in a predictable way."

  (make-pathname :name (string-downcase (package-name package))
                 :type "html"))


(defun document-package (name &optional (base-path #P"docs/"))
  "Generates documentation in html form for package named by :param:`name`."

  (let* ((package (find-package name))
         (pathname (merge-pathnames (package-path package) base-path))
         (coo.roles:*context-package* package))

    (with-open-file (s pathname :direction :output :if-exists :supersede :if-does-not-exist :create)
      (docutils:write-html s (docutils:read-rst (package-node package))))))


(defun make-title (title stream &key (level 0))
  "Write out a title in rst with markers appropriate for the specified title level."

  (let ((marker (make-string (+ 4 (length title)) :initial-element (nth level +headings+))))
    (format stream "~a~%~a~%~a~%"
	    marker
	    title
	    marker)))


(defun render-section (title type specs stream)
  (make-title title stream :level 1)
  (dolist (spec specs)
    (destructuring-bind (symbol . documentation) spec
      (format stream "~&.. _~a-~a:

``~a``
  ~a~%~%"
              type
              (coo.roles:anchor-name-for-symbol symbol)
              (string-downcase (symbol-name symbol))
              documentation))))


(defun package-node (package)
  (uiop:with-temporary-file (:stream s
			     :pathname path
			     :keep t
                             :type "rst"
			     :direction :output
			     :element-type 'character)
    (let ((coo.roles:*context-package* package))
      ;; Start with the title of the package
      (make-title (str:concat (string-downcase (package-name package)) " package") s)
      ;; Write the package doumentation if it's defined
      (if-let ((pkg-docs (documentation package t)))
	(format s "~%~a~%~%" pkg-docs)
        (format s "~%~%"))

      (let (variables macros classes functions)
	(do-external-symbols (symbol package)
	  (cond
	    ((fboundp symbol)
	     (if (macro-function symbol)
		 (push (cons symbol (documentation (symbol-function symbol) t)) macros)
		 (push (cons symbol (documentation (symbol-function symbol) 'function)) functions)))
	    ((find-class symbol nil)
	     (push (cons symbol (documentation (find-class symbol) t)) classes))
	    ((boundp symbol)
	     (push (cons symbol (documentation symbol 'variable)) variables))))
	(when variables
	  (render-section "Variables" "variable" variables s))
	(when macros
	  (render-section "Macros" "macro" macros s))
	(when classes
	  (render-section "Classes" "class" classes s))
	(when functions
	  (render-section "Functions" "function" functions s)))
      path)))


(defconstant +sub-package-separators+
  '(#\/ #\.)
  "List of characters that could be used to separate a base package name like ``cool-package`` from sub-packages like ``cool-package.types`` or ``cool-package/types``.")


(defun make-sub-package-matcher (system-name)
  "Creates a predicate function that determines if a package is likely a child of the system named :param:`system-name`."

  (let ((regex (ppcre:create-scanner (format nil "^~a[~{~a~}].*" system-name +sub-package-separators+)
                                     :case-insensitive-mode t)))
    (lambda (candidate)
      (or (string-equal system-name candidate)
          (ppcre:scan regex candidate)))))


(defun discover-system-packages (system-name)
  "Based on the assumption that all packages a system defines should start with that system's own name, find all the packages that are likely children."

  (->> (list-all-packages)
       (mapcar #'package-name)
       (remove-if-not (make-sub-package-matcher system-name))))


(defun document-system (system &key (base-path #P"docs/") (discover-packages t) packages)
  "Generate documentation for :param:`system` in :param:`base-path`.

If :param:`discover-packages` is true (the default), it will try to figure out all the packages that are likely defined by the system and generate documentation for all of them, too.  If :param:`discover-packages` is ``nil``, it will fall-back to whatever packages are defined by :param:`packages`."

  (unless (typep system 'asdf:system)
    (setq system (asdf:find-system system)))

  (let ((index-path (merge-pathnames #P"index.html" base-path)))
    (uiop:ensure-all-directories-exist (list index-path))
    (with-open-file (s index-path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (docutils:write-html s (docutils:read-rst (system-node system
                                                             (-> (if discover-packages
                                                                     (discover-system-packages (asdf:component-name system))
                                                                     packages)
                                                                 (sort #'string<))
                                                             base-path)))))

  (dolist (package-name (if discover-packages (discover-system-packages (asdf:component-name system)) packages))
      (let ((package (find-package package-name)))
        (document-package package base-path)))

  ;; Copy styles to new directory
  (uiop:copy-file #P"default.css" (merge-pathnames #P"default.css" base-path)))


(defun system-collect-metadata (system stream)
  "Collect various metadata about a system such as a the author, license, &c."

  (macrolet ((info (key)
               (with-gensyms (var)
                 `(when-let ((,var (,(read-from-string (str:concat "ASDF:SYSTEM-" (symbol-name key))) system)))
                    (format stream "~&~15a~a~%" ,(string-downcase (str:concat ":" (symbol-name key) ":")) ,var)))))

    (info author)
    (when-let ((version (asdf:component-version system)))
      (format stream "~&~15a~a~%" ":version:" version))
    (info license)
    (info homepage)))


(defun system-node (system packages base-path)
  (uiop:with-temporary-file (:stream s
                             :pathname path
                             :direction :output
                             :keep t
                             :type "rst"
                             :element-type 'character)
    (make-title (asdf:component-name system) s)

    (system-collect-metadata system s)

    (format s "~&~a~%~%"
            (or (asdf:system-long-description system)
                (asdf:system-description system)
                ""))

    (make-title "Packages" s :level 1)
    (dolist (package-name packages)
      (let ((package (find-package package-name)))
        (format s "~&* `~a <~a>`_~%~%"
                (string-downcase package-name)
                (package-path package))
        ;; (format s "~%`~a <~a>`_~%  ~a~%~%"
        ;;         (string-downcase package-name)
        ;;         (package-path package)
        ;;         (or (documentation package t) ""))
        ))

    path))
