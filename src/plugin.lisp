(defpackage :coo.plugin
  (:use :cl :alexandria)
  (:export #:initialize-plugins
           #:config
           #:metadata-for-symbol))

(in-package :coo.plugin)

(defvar *plugin* nil
  "The active plugin in hooks.  A string.")


(defun initialize-plugins ()
  (coo.config:get-value "plugins"))


(defun config (key &optional default)
  (assert (not (null *plugin*)) nil "config called with *plugin* unbound.")

  (when-let ((plugins (coo.config:get-value "plugins")))
    (when-let ((plugin-config (gethash *plugin* plugins)))
      (gethash key plugin-config default))))


(defun find-hook-in-plugin (hook)
  (find-symbol hook (find-package (format nil "COO.PLUGINS.~:@(~a~)" *plugin*))))


(defun metadata-for-symbol (system node)
  (when (coo.config:get-value "plugins")
    (with-output-to-string (out)
      (loop for plugin being the hash-key of (coo.config:get-value "plugins")
            do (let ((*plugin* plugin))
                 (funcall (find-hook-in-plugin "METADATA") out system node))))))
