(asdf:defsystem #:o2-game
  :description "Operation 'Operation' is 2D shooter about war behind enemy lines."
  :author "Pavel Kulyov <kulyov.pavel@gmail.com>, Ekaterina Vaartis <vaartis@cock.li>"
  :license "MIT"
  :depends-on (:sdl2
               :sdl2-image
               :sdl2-ttf
               :local-time
               #+slynk
               :livesupport
               :chipmunk
               :o2)
  :pathname "o2-game"
  :components ((:file "package")
               (:file "o2")
               (:file "ui")
               (:file "charge")
               (:file "components")
               (:file "james")
               (:file "enemy")
               (:file "weapon")
               (:file "ingame")
               (:file "menu")
               (:file "application"))

  :build-operation "asdf:program-op"
  :build-pathname "../o2-game"
  :entry-point "o2-game:main")
