(asdf:defsystem #:o2
  :description "O2 game engine."
  :author "Pavel Kulyov <kulyov.pavel@gmail.com>, Ekaterina Vaartis <vaartis@cock.li>"
  :license "MIT"
  :depends-on (:sdl2
               :sdl2-image
               :sdl2-ttf
               #+slynk
               :livesupport
               :chipmunk
               :log4cl)
  :pathname "o2"
  :components ((:file "package")
               (:file "utils")
               (:file "events")
               (:file "project")
               (:file "assets")
               (:file "game-object")
               (:file "application")
               (:file "renderer")
               (:file "state")
               (:file "component")
               (:file "system")))

(asdf:defsystem #:o2/examples
  :description "O2 game engine usage examples."
  :author "Pavel Kulyov <kulyov.pavel@gmail.com>"
  :license "MIT"
  :depends-on (:o2)
  :pathname "examples"
  :components ((:file "package")
               (:file "simple")))
