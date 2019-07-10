(asdf:defsystem :o2
  :description "Operation 'Operation' is 2D shooter about war behind enemy lines."
  :author "Pavel Kulyov <kulyov.pavel@gmail.com>, Ekaterina Vaartis <vaartis@cock.li>"
  :license "MIT"
  :depends-on (:sdl2
               :sdl2-image
               :sdl2-ttf
               :local-time
               #+slynk
               :livesupport
               :chipmunk)
  :pathname "src"
  :components ((:file "o2")
               (:file "package")
               (:file "components")
               (:file "gobject")
               (:file "ui")
               (:file "charge")
               (:file "james")
               (:file "enemy")
               (:file "weapon")
               (:file "renderer")
               (:file "state")
               (:file "ingame")
               (:file "application")
               (:file "menu"))
  :build-operation "asdf:program-op"
  :build-pathname "../o2"
  :entry-point "o2:main")
