#| Useful utilities.

|#

(in-package :o2/engine)


(defmacro continuable (&body body)
  "A macro to disable livesupport when in SLIME/SLY."
  #+slynk
  `(livesupport:continuable ,@body)
  #-slynk
  `(progn ,@body))
