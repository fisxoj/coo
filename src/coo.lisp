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


(defparameter +headings+ '(#\= #\- #\_ #\~ #\^)
  "Different symbols that can be used to frame headers in ReST.")


(djula:add-template-directory (asdf:system-relative-pathname :coo "templates/"))


(defun document-package (package-index &optional (base-path #P"docs/"))
  "Generates documentation in html form for :param:`package-index`."

  (let* ((pathname (make-pathname :defaults base-path
                                  :name (-> package-index docparser:package-index-name string-downcase)
                                  :type "rst"
                                  :version nil))
         (coo.roles:*context-package* (find-package (docparser:package-index-name package-index)))
         (args (list :variables nil
                     :functions nil
                     :macros nil
                     :generic-functions nil
                     :structures nil
                     :classes nil)))

    (docparser:do-nodes (node package-index)
      (when (docparser:symbol-external-p (docparser:node-name node))
        (typecase node
          (docparser:variable-node (push node (getf args :variables)))
          (docparser:function-node (push node (getf args :functions)))
          (docparser:macro-node (push node (getf args :macros)))
          (docparser:generic-function-node (push node (getf args :generic-functions)))
          (docparser:struct-node (push node (getf args :structures)))
          (docparser:class-node (push node (getf args :classes)))
          (t (warn "Not handling node of type ~a" (class-of node))))))

    (with-open-file (s pathname :direction :output :if-exists :supersede :if-does-not-exist :create)
      (apply #'djula:render-template* "package-index.rst" s
             :package package-index
             args))

    (with-open-file (s (make-pathname :defaults pathname :type "html") :direction :output :if-exists :supersede :if-does-not-exist :create)
      (docutils:write-html s (docutils:read-rst pathname)))))


(defun make-title (title stream &key (level 0))
  "Write out a title in rst with markers appropriate for the specified title level."

  (let ((marker (make-string (+ 4 (length title)) :initial-element (nth level +headings+))))
    (format stream "~a~%~a~%~a~%"
	    marker
	    title
	    marker)))


(djula::def-filter :title (it &optional (level 0))
  (make-title it nil :level level))


(defgeneric linkify (object stream)
  (:documentation "Generate a relative url to this thing.")

  (:method ((object docparser:package-index) stream)
    (coo.util:make-url stream (docparser:package-index-name object)))

  (:method ((object docparser:name-node) stream)
    (let* ((symbol (docparser:node-name object))
           (class-name (-> object class-of class-name string-downcase))
           (node-type (if (str:ends-with-p "-node" class-name)
                          (subseq class-name 0 (- (length class-name) (length "-node")))
                          class-name)))

      (coo.util:make-url stream (symbol-package symbol) (symbol-name symbol) node-type))))


(defgeneric anchorfy (object stream)
  (:documentation "Generate an anchor reference in rst to this thing.")

  (:method ((object docparser:name-node) stream)
    (let* ((symbol (docparser:node-name object))
           (class-name (-> object class-of class-name string-downcase))
           (node-type (if (str:ends-with-p "-node" class-name)
                          (subseq class-name 0 (- (length class-name) (length "-node")))
                          class-name)))

      (coo.util:make-anchor stream (symbol-name symbol) node-type))))


(djula::def-filter :linkify (it)
  (linkify it nil))


(djula::def-filter :anchorfy (it)
  (anchorfy it nil))

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


(defun document-system (system &key (base-path #P"docs/"))
  "Generate documentation for :param:`system` in :param:`base-path`.

If :param:`discover-packages` is true (the default), it will try to figure out all the packages that are likely defined by the system and generate documentation for all of them, too.  If :param:`discover-packages` is ``nil``, it will fall-back to whatever packages are defined by :param:`packages`."

  (unless (typep system 'asdf:system)
    (setq system (asdf:find-system system)))

  (let ((index-path (merge-pathnames #P"index.rst" base-path))
        (index (docparser:parse system))
        ;; Disable auto-escape because it's for html and we're using ReST
        (djula:*auto-escape* nil)
        (djula:*catch-template-errors-p* nil))

    (uiop:ensure-all-directories-exist (list index-path))
    (uiop:temporary-directory)
    (with-open-file (s index-path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (djula:render-template* "index.rst" s
                              :system system
                              :index index))

    (with-open-file (s (make-pathname :defaults index-path :type "html") :direction :output :if-exists :supersede :if-does-not-exist :create)
      (docutils:write-html s (docutils:read-rst index-path)))


    (docparser:do-packages (package index)
      (document-package package base-path)))

  ;; Copy styles to new directory
  (uiop:copy-file (asdf:system-relative-pathname :coo "default.css")
                  (merge-pathnames #P"default.css" base-path)))


;; (defun system-collect-metadata (system stream)
;;   "Collect various metadata about a system such as a the author, license, &c."

;;   (macrolet ((info (key)
;;                (with-gensyms (var)
;;                  `(when-let ((,var (,(read-from-string (str:concat "ASDF:SYSTEM-" (symbol-name key))) system)))
;;                     (format stream "~&~15a~a~%" ,(string-downcase (str:concat ":" (symbol-name key) ":")) ,var)))))

;;     (info author)
;;     (when-let ((version (asdf:component-version system)))
;;       (format stream "~&~15a~a~%" ":version:" version))
;;     (info license)
;;     (info homepage)))


;; (defun system-node (system packages base-path)
;;   (uiop:with-temporary-file (:stream s
;;                              :pathname path
;;                              :direction :output
;;                              :keep t
;;                              :type "rst"
;;                              :element-type 'character)
;;     (make-title (asdf:component-name system) s)

;;     (system-collect-metadata system s)

;;     (format s "~&~a~%~%"
;;             (or (asdf:system-long-description system)
;;                 (asdf:system-description system)
;;                 ""))

;;     (make-title "Packages" s :level 1)
;;     (dolist (package-name packages)
;;       (let ((package (find-package package-name)))
;;         (format s "~&* `~a <~a>`_~%~%"
;;                 (string-downcase package-name)
;;                 (package-path package))
;;         ;; (format s "~%`~a <~a>`_~%  ~a~%~%"
;;         ;;         (string-downcase package-name)
;;         ;;         (package-path package)
;;         ;;         (or (documentation package t) ""))
;;         ))

;;     path))
