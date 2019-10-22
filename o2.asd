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
