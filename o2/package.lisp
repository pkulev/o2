(defpackage :o2
  (:documentation "O2 main package.")
  (:use :cl)
  (:export #:main
           #:*core-is-root*)
  (:import-from :o2/engine :requires))
