(defpackage coo.introspection
  (:use #:cl #:alexandria #:cl-arrows)
  (:export #:find-definition-line-number))

(in-package :coo.introspection)


(defun absolute-path-to-system-relative (system pathname)
  "Create a path relative to SYSTEM pointing to PATHNAME.  Will only work on paths that are subdirectories of the path of SYSTEM."

  (let ((absolute-directory (pathname-directory pathname))
        (system-directory (pathname-directory (asdf:system-relative-pathname system ""))))
    (make-pathname :defaults pathname
                   :directory (list* :relative (set-difference absolute-directory system-directory :test 'string-equal)))))


(defun count-lines-up-to-character (pathname character-count)
  "Reads through PATHNAME and counts the number of newlines before reaching CHARACTER-COUNT."

  (with-open-file (s pathname)
    (loop for count from 0 by 1
          with line = 1
          with maybe-eat-lf = nil
          for char = (read-char s)

          ;; do (format t "~& ~5,d ~5,d: ~@C ~a" count line char maybe-eat-lf)

          do (cond
               ((char= #\Newline char)
                (incf line)
                (setf maybe-eat-lf t))

               ((char= #\Linefeed char)
                (if maybe-eat-lf
                    (setf maybe-eat-lf nil)
                    (incf line)))

               (t (when maybe-eat-lf
                    (setf maybe-eat-lf nil))))

          until (> count character-count)

          finally (return line))))


(defun find-definition-line-number (system object)
  #+sbcl(sbcl-find-definition-line-number system object))


#+sbcl
(defun sbcl-find-definition-line-number (system object)
  (let* ((definition-source (sb-introspect:find-definition-source object))
         (pathname (sb-introspect:definition-source-pathname definition-source))

         (line-number (if-let ((character-count (sb-introspect:definition-source-character-offset definition-source)))
                        (count-lines-up-to-character pathname character-count)
                        0)))

    (values (absolute-path-to-system-relative system pathname) line-number)))
