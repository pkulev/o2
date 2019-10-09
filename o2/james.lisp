(in-package :o2)

(defun make-james (&key (position (cons 0 0)) sprite sitting-sprite physical-space)
  (let* ((dfloat-width (coerce (o2/engine:sprite-width :player) 'double-float))
         (dfloat-height (coerce (o2/engine:sprite-height :player) 'double-float))
         (mass 60.0d0)
         (moment (chipmunk:moment-for-box mass dfloat-width dfloat-height))
         (rigid-body (chipmunk:add physical-space (chipmunk:make-body
                                                   mass
                                                   moment)))
         (shape (chipmunk:add physical-space (chipmunk:make-box-shape
                                              rigid-body
                                              dfloat-width
                                              dfloat-height
                                              0d0)))
         (player
           (o2/engine:make-game-object
            :components (o2/engine:player-tag
                         o2/engine:player-controlable-c
                         invincibility-c
                         (health-c
                          :death-action (lambda ()
                                          (o2/engine:push-event :restart-current-state)))
                         (o2/engine:transform-c :position position)
                         (o2/engine:physical-c :rigid-body rigid-body :shape shape)
                         (o2/engine:render-c :sprite :player :render-priority 2)
                         (o2/engine:shooter-c
                          :bullet-collision-type :player-bullet
                          :bullet-shape-filter (chipmunk:make-shape-filter
                                                '(:player :player-bullet) '(:enemy))
                          :weapons
                          (list
                           (make-instance 'G17
                                          :components (list
                                                       (make-instance 'o2/engine:transform-c))
                                          :ammo 32
                                          :ammo-in-mag 17
                                          :mag-capacity 17
                                          :chambered? t
                                          :sprite :G17))
                          :current-weapon 'G17))
            :systems (health-system o2/engine:physical-system o2/engine:player-controlable-system
                                    o2/engine:render-system o2/engine:shooter-system))))

    (setf (chipmunk:user-data shape) (cffi:make-pointer (o2/engine:id player)))
    (setf (chipmunk:collision-type shape) :player)
    (setf (chipmunk:shape-filter shape) (chipmunk:make-shape-filter '(:player) '(:ground :enemy-bullet)))

    (chipmunk:with-collision-handler-for (handler (physical-space :player :ground))
      (chipmunk:define-collision-begin-callback player-ground-collision (arbiter physical-space data)
        (declare (ignorable arbiter physical-space data))
        (let* ((pl-move-c (o2/engine:find-component player 'o2/engine:player-controlable-c)))
          (with-accessors ((jumping? o2/engine:jumping?)) pl-move-c
            (when jumping? (setf jumping? nil))))

        1)
      (setf (chipmunk:begin-collision-fun handler) 'player-ground-collision))

    (o2/engine:add-child
     player
     (o2/engine:add-object (o2/engine:current-app-state)
                           (o2/engine:make-game-object
                            :components
                            (o2/engine:transform-c
                             (o2/engine:render-c :render-priority 2)
                             (text-widget-c
                              :data-getter
                              (lambda ()
                                (with-accessors ((player o2/engine:actor))
                                    (o2/engine:current-app-state)
                                  (let ((health-c (o2/engine:find-component player 'health-c))
                                        (invinc-c (o2/engine:find-component player 'invincibility-c)))
                                    (format
                                     nil
                                     "~A/~A ~A"
                                     (health health-c)
                                     (max-health health-c)
                                     (if
                                      (and invinc-c (is-invincible invinc-c)) "[I]" "")))))))
                            :systems (text-widget-system))))

    (setf (chipmunk:friction shape) 0.5d0)
    (let ((dfloat-x (coerce (car position) 'double-float))
          (dfloat-y (coerce (cdr position) 'double-float)))
      (setf (chipmunk:position rigid-body) (chipmunk:make-cp-vect dfloat-x dfloat-y)))
    player))
