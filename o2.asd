(asdf:defsystem :o2
  :description "Operation 'Operation' is 2D shooter about war behind enemy lines."
  :author "Pavel Kulyov <kulyov.pavel@gmail.com>, Ekaterina Vaartis <vaartis@cock.li>"
  :license "MIT"
  :depends-on (:sdl2
               :sdl2-image
               :sdl2-ttf
               :local-time
               #+slynk
               :livesupport)
  :pathname "src"
  :components ((:file "package")
               (:file "o2")
               (:file "gobject")
               (:file "ui")
               (:file "background")
               (:file "charge")
               (:file "james")
               (:file "enemy")
               (:file "weapon")
               (:file "renderer")
               (:file "state")
               (:file "ingame")
               (:file "menu")
               (:file "application"))
  :build-operation "asdf:program-op"
  :build-pathname "../o2"
  :entry-point "o2:main")
