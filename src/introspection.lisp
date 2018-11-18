(defpackage coo.introspection
  (:use #:cl #:alexandria #:cl-arrows)
  (:export #:find-definition-line-number))

(in-package :coo.introspection)


(defun absolute-path-to-system-relative (system pathname)
  "Create a path relative to :param:`system` pointing to :param:`pathname`.  Will only work on paths that are subdirectories of the path of :param:`system`."

  (let ((absolute-directory (pathname-directory pathname))
        (system-directory (pathname-directory (asdf:system-relative-pathname system ""))))
    (make-pathname :defaults pathname
                   :directory (list* :relative (set-difference absolute-directory system-directory :test 'string-equal)))))


(defun count-lines-up-to-character (pathname character-count)
  "Reads through :param:`pathname` and counts the number of newlines before reaching :param:`character-count`."

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

          finally (return (1+ line)))))


(defun find-definition-line-number (system object)
  "Returns ``(values relative-path line-number)``, for a given reference.  Uses implementation-specific functions to try to get at the info or returns ``(values nil nil)`` if unimplemented."

  #+sbcl(sbcl-find-definition-line-number system object)
  #-sbcl(values nil nil))


#+sbcl
(defun sbcl-find-definition-line-number (system object)
  (let* ((definition-source (sb-introspect:find-definition-source object))
         (pathname (sb-introspect:definition-source-pathname definition-source))

         (line-number (if-let ((character-count (sb-introspect:definition-source-character-offset definition-source)))
                        (count-lines-up-to-character pathname character-count)
                        0)))

    (values (absolute-path-to-system-relative system pathname) line-number)))
