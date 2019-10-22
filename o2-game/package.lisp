(defpackage #:o2-game
  (:documentation "O2 game main package.")
  (:use :cl)
  (:export #:main
           #:*core-is-root*)
  (:import-from :o2/engine :requires))
