(asdf:defsystem :o2
  :description "Operation 'Operation' is 2D shooter about war behind enemy lines."
  :author "Pavel Kulyov <kulyov.pavel@gmail.com>, Ekaterina Vaartis <vaartis@cock.li>"
  :license "MIT"
  :depends-on (:sdl2
               :sdl2-image
               :livesupport)
  :pathname "src"
  :components ((:file "package")
               (:file "o2")
               (:file "gobject")
               (:file "background")
               (:file "collision")
               (:file "renderer")
               (:file "state")
               (:file "ingame")
               (:file "application")))
