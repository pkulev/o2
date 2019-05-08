(in-package :o2)

(defun main (&key (core-is-root nil))
  (defparameter +root+
    (if core-is-root
        *default-pathname-defaults*
        (asdf:system-source-directory :o2))
    "Project root absolute path.")

  (let ((o2 (make-application)))
    (start o2)))
