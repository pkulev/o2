(defpackage #:o2/engine
  (:documentation "o2 game engine.")
  (:use :cl)
  (:shadow :position)
  (:export
   ;; Application
   :*application*  ; FIXME: don't touch it directly please!
   :get-current-application
   :application
   :frame-ms
   :states
   :current-state
   :renderer
   :current-app-state
   :register-state
   :deregister-state
   :set-state
   :get-state
   :start
   :stop

   ;; events
   :register-event-type
   :push-event

   ;; state API
   :add-object
   :actor
   :state
   :init
   ;; TODO: move this out -->
   :score
   :physical-space
   ;; TODO: move this out <--
   :set-state
   :update
   :objects
   :register-state
   :deregister-state
   :actor
   :running?
   :cleanup

   ;; game object API
   :id
   :game-object
   :make-game-object
   :find-object-by-id
   :destroy
   :add-child
   :*game-object*

   ;; component API
   :*physical-component-cleanups*
   :component
   :components
   :children
   :find-component
   :flip
   :rigid-body
   :position
   :camera-tag
   :transform-c
   :render-c
   :physical-c
   :player-tag
   :player-controlable-c
   :global-position
   :local-to-global-position
   :find-with-component
   :shooter-c
   :parent

   :bullet-shape-filter
   :bullet-collision-type

   :reload?
   :weapons
   :current-weapon
   :shoot?

   :jumping?

   ;; system API
   :system
   :run-systems
   :run-system
   :render-system
   :physical-system
   :player-controlable-system
   :shooter-system
   :camera-system

   ;; renderer API
   :set-current-renderer
   :sprite
   :add-font
   :add-sprite
   :sprite-width
   :sprite-height
   :renderer
   :draw-rect
   :draw-text

   ;; utils
   :continuable))
