(in-package :o2)

(defparameter *core-is-root* nil
  "Should the image file be the root of the project,
   or should it be determined at runtime (switch to t for packaging)")

(defun main ()
  (defparameter +root+
    (if *core-is-root*
        *default-pathname-defaults*
        (asdf:system-source-directory :o2))
    "Project root absolute path.")

  (let ((o2 (make-application)))
    (o2/engine:start o2)))
