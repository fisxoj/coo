(defpackage coo
  (:use #:cl
        #:alexandria)
  (:import-from #:docutils
                #:add-child)
  (:export #:document-package))

(in-package :coo)

(defun document-package (name)
  "Generates documentation in html form for package named by :param:`name`."

  (let* ((package (find-package name))
         (pathname (make-pathname :name (string-downcase (package-name package))
                                  :type "html"))
         (document (docutils:make-node 'docutils:document))
         (package-node (package-node package)))
    (with-open-file (s (print pathname) :direction :output :if-exists :supersede :if-does-not-exist :create)
      (add-child document package-node)
      (docutils:write-html s document))))


(defun make-title (title)
  (let ((title-node (docutils:make-node 'docutils.nodes:title)))
    (add-child title-node (docutils:make-node 'docutils.nodes:text :text title))
    title-node))


(defun reparent-node (parent child)
  (slot-makunbound child 'docutils:parent)
  (add-child parent child))


(defun inner-nodes (rst-text)
  (slot-value (docutils:read-rst rst-text) 'docutils::children))


(defun render-thing (spec type)
  (destructuring-bind (symbol . documentation) spec
    (let ((section (docutils:make-node 'docutils.nodes:section :attributes (list :name (format nil "~@[~a__~]~a" type (string-downcase symbol))))))
      (add-child section (make-title (string-downcase (symbol-name symbol))))
      (dolist (child (inner-nodes documentation)) (reparent-node section child))

      (values section))))

(defun render-section (title type specs)
  (let ((section (docutils:make-node 'docutils.nodes:section :attributes (list :name (format nil "~@[~a__~]~a" type title)))))
    (add-child section (make-title title))
    (dolist (thing specs) (add-child section (render-thing thing type)))
    section))


(defun package-node (package)
  (let ((coo.roles:*context-package* package)
        (section (docutils:make-node 'docutils.nodes:section :attributes (list :id (string-downcase (package-name package)) :name (string-downcase (package-name package)))))
        (children (inner-nodes (documentation package t))))
    (add-child section (make-title (concatenate 'string (string-downcase (package-name package)) " package")))
    (dolist (child children) (reparent-node section child))

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
      (add-child section (render-section "Variables" "variable" variables))
      (add-child section (render-section "Macros" "macro" macros))
      (add-child section (render-section "Classes" "class" classes))
      (add-child section (render-section "Functions" "function " functions)))
    section))
