(defpackage #:o2/engine
  (:documentation "o2 game engine.")
  (:use :cl)
  (:shadow :position)
  (:export :application  ; application API
           :actor
           :*application*
           :add-event-type
           :current-app-state
           :current-state
           :set-current-renderer
           :register-state
           :+delay+
           :states
           :cleanup
           :continuable
           :push-event

           ;; state API
           :add-object
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
           :sprite
           :add-font
           :add-sprite
           :sprite-width
           :sprite-height
           :renderer
           :draw-rect
           :draw-text))
