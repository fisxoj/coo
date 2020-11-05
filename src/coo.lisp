(defpackage coo
  (:use #:cl
        #:cl-arrows
        #:alexandria)
  (:import-from #:docutils
                #:add-child)
  (:export #:document-system)
  (:documentation "Generally, you'll want to use :function:`document-system` to start yourself off.

try running it like this::

    CL-USER> (ql:quickload :coo)
    CL-USER> (coo:document-system :my-cool-system)
"))

(in-package :coo)


(defparameter +headings+ '(#\= #\- #\_ #\~ #\^)
  "Different symbols that can be used to frame headers in ReST.")


(defvar *system* nil
  "The system being processed.")


(djula:add-template-directory (asdf:system-relative-pathname :coo "templates/"))


(defun make-root-dir (package-name)
  "Takes a string with the package name and returns another with the path
   to the root of the documentation.

   We need this because docs for package inferred packages live in the nested
   subdirectories.

   Here are examples of the output:

   (make-root-dir-path \"foo\") -> \"\"
   (make-root-dir-path \"foo/bar/baz\") -> \"../../\"
"
  (check-type package-name string)
  
  (let ((slash-count (count #\/ package-name)))
    (str:repeat slash-count
                "../")))


(defun document-package (package-index system &key keep-rst (base-path #P"docs/"))
  "Generates documentation in html form for :param:`package-index`.

The documentation file will have the pathanme ``{{base-path}}{{package-name}}.html``, so a package named ``cool-package`` with :param:`base-dir` ``docs/`` will have the generated pathame ``docs/cool-package.html``.

If :param:`keep-rst` is truthy, don't delete the intermediate restructured text file."

  (let* ((package-name (-> package-index docparser:package-index-name string-downcase))
         (pathname (merge-pathnames 
                    ;; In package inferred ASDF systems
                    ;; package names may include / as filenames
                    ;; they belong to. Thus we have to treat
                    ;; package-name as a pathname
                    (uiop:parse-unix-namestring package-name :type "rst")
                    base-path))
         (coo.roles:*context-package* (find-package (docparser:package-index-name package-index)))
         (reader (make-instance 'docutils.parser.rst:rst-reader
                                :settings '((:warnings . #P"docutils-warnings.log")
                                            (:generator . nil)
                                            (:resolve-media . nil))))
         (args (list :variables nil
                     :functions nil
                     :macros nil
                     :generic-functions nil
                     :structures nil
                     :classes nil)))
    (coo.util:with-root ((make-root-dir package-name))
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

      (unwind-protect
	   (progn
             (ensure-directories-exist pathname)
           
	     (with-open-file (s pathname :direction :output :if-exists :supersede :if-does-not-exist :create)
               (let ((*system* system))
                 (apply #'djula:render-template* "package-index.rst" s
                        :root-dir (coo.util:root)
                        :system system
                        :package package-index
                        args)))

	     (with-open-file (s (make-pathname :defaults pathname :type "html") :direction :output :if-exists :supersede :if-does-not-exist :create)
	       (let ((rst-document (docutils:read-document pathname reader)))
                 (setf (docutils:setting :stylesheet rst-document)
                       (concatenate 'string
                                    (coo.util:root)
                                    "default.css"))
                 (docutils:write-html s rst-document))))

        (unless keep-rst
	  (uiop:delete-file-if-exists pathname))))))


(defun make-title (title stream &key (level 0))
  "Write out a title in rst with markers appropriate for the specified title level."

  (let ((marker (make-string (+ 4 (length title)) :initial-element (nth level +headings+))))
    (format stream "~a~%~a~%~a~%"
	    marker
	    title
	    marker)))


(djula::def-filter :title (it &optional (level 0))
  (let ((level (if (integerp level) level (parse-integer level))))
    (make-title it nil :level level)))


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


(djula::def-filter :metadata (node)
  (coo.plugin:metadata-for-symbol *system* node))


(djula::def-filter :remove-unintentional-whitespace (it)
  "Renders a paragraph of text (probably a docstring) with any single leading spaces on each line trimmed.  This prevents errors in the restructured text, where leading whitespace can be meaningful, but allows lisp projects that don't use ReST in comments to process their line-broken docstrings."

  (flet ((trim-at-most-one (line)
	   (if (and (> (length line) 2)
		    (char= (char line 0) #\Space)
		    (char/= (char line 1) #\Space))
	       (subseq line 1)
	       line)))

    (str:unlines (mapcar #'trim-at-most-one (str:lines it)))))


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


(defun document-system (system &key keep-rst (base-path #P"docs/"))
  "Generate documentation for :param:`system` in :param:`base-path`.

If :param:`keep-rst` is truthy, don't delete the intermediate restructured text file."

  (unless (typep system 'asdf:system)
    (setq system (asdf:find-system system)))

  (let ((index-path (merge-pathnames #P"index.rst" base-path))
        (index (docparser:parse system))
        ;; Customized RST-writer for docutils to muffle some errors
        (reader (make-instance 'docutils.parser.rst:rst-reader
                               :settings '((:warnings . #P"docutils-warnings.log")
                                           (:generator . nil)
                                           (:resolve-media . nil))))
        ;; Disable auto-escape because it's for html and we're using ReST
        (djula:*auto-escape* nil)
        (djula:*catch-template-errors-p* nil))

    (uiop:ensure-all-directories-exist (list index-path))
    (uiop:temporary-directory)
    (unwind-protect
	 (progn
	   (with-open-file (s index-path :direction :output :if-exists :supersede :if-does-not-exist :create)
	     (djula:render-template* "index.rst" s
				     :system system
				     :index index))

	   (with-open-file (s (make-pathname :defaults index-path :type "html") :direction :output :if-exists :supersede :if-does-not-exist :create)
	     (docutils:write-html s (docutils:read-document index-path reader))))

      (unless keep-rst
	(uiop:delete-file-if-exists index-path)))


    (docparser:do-packages (package index)
      (document-package package system
			:keep-rst keep-rst
			:base-path base-path)))

  ;; Copy styles to new directory
  (uiop:copy-file (asdf:system-relative-pathname :coo "default.css")
                  (merge-pathnames #P"default.css" base-path)))
