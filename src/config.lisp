(defpackage :coo.config
  (:use :cl :alexandria)
  (:export #:get-value))

(in-package :coo.config)

(defun get-value (key &optional default)
  (let ((config-file-pathname (merge-pathnames "coo.toml")))

    (when (uiop:file-exists-p config-file-pathname)
      (when-let ((configuration (pp-toml:parse-toml (alexandria:read-file-into-string config-file-pathname))))
        (gethash key configuration default)))))
