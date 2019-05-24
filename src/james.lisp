(in-package :o2)

(defun make-james (&key (position (cons 0 0)) sprite sitting-sprite space)
  (let* ((dfloat-width (coerce (sprite-width :player) 'double-float))
         (dfloat-height (coerce (sprite-height :player) 'double-float))
         (mass 60.0d0)
         (moment (chipmunk:moment-for-box mass dfloat-width dfloat-height))
         (rigid-body (chipmunk:add space (chipmunk:make-body
                                          mass
                                          moment)))
         (shape (chipmunk:add space (chipmunk:make-box-shape
                                     rigid-body
                                     dfloat-width
                                     dfloat-height
                                     0d0)))
         (player (make-instance
                  'game-object
                  :components (list
                               (make-instance 'player-tag)
                               (make-instance 'health-c
                                              :death-action (lambda ()
                                                              (push-event :restart-current-state)))
                               (make-instance 'player-controlable-c)
                               (make-instance 'transform-c :position position)
                               (make-instance 'physical-c
                                              :rigid-body rigid-body
                                              :shape shape)
                               (make-instance 'render-c
                                              :sprite :player
                                              :render-priority 2)
                               (make-instance 'shooter-c
                                              :bullet-collision-type :player-bullet
                                              :bullet-shape-filter (chipmunk:make-shape-filter
                                                                    '(:player :player-bullet) '(:enemy))
                                              :weapons (list
                                                        (make-instance 'G17
                                                                       :components (list
                                                                                    (make-instance 'transform-c))
                                                                       :ammo 32
                                                                       :current-ammo 17
                                                                       :sprite :G17))
                                              :current-weapon 'G17)
                               (make-instance 'invincibility-c))
                  :systems (list
                            (make-instance 'health-system)
                            (make-instance 'physical-system)
                            (make-instance 'player-controlable-system)
                            (make-instance 'render-system)
                            (make-instance 'shooter-system)))))

    (setf (chipmunk:user-data shape) (cffi:make-pointer (id player)))
    (setf (chipmunk:collision-type shape) :player)
    (setf (chipmunk:shape-filter shape) (chipmunk:make-shape-filter '(:player) '(:ground :enemy-bullet)))

    (chipmunk:with-collision-handler-for (handler (space :player :ground))
      (chipmunk:define-collision-begin-callback player-ground-collision (arbiter space data)
        (declare (ignorable arbiter space data))
        (let* ((pl-move-c (find-component player 'player-controlable-c)))
          (with-accessors ((jumping? jumping?)) pl-move-c
            (when jumping? (setf jumping? nil))))

        1)
      (setf (chipmunk:begin-collision-fun handler) 'player-ground-collision))

    (add-child
     player
     (add-object (current-app-state)
                 (make-instance 'game-object
                                :components
                                (list
                                 (make-instance 'transform-c)
                                 (make-instance 'render-c :render-priority 2)
                                 (make-instance 'text-widget-c
                                                :data-getter
                                                (lambda ()
                                                  (with-accessors ((player actor)) (current-app-state)
                                                    (let ((health-c (find-component player 'health-c))
                                                          (invinc-c (find-component player 'invincibility-c)))
                                                      (format
                                                       nil
                                                       "~A/~A ~A"
                                                       (health health-c)
                                                       (max-health health-c)
                                                       (if
                                                        (and invinc-c (is-invincible invinc-c)) "[I]" "")))))))
                                :systems
                                (list
                                 (make-instance 'text-widget-system)))))

    (setf (chipmunk:friction shape) 0.5d0)
    (let ((dfloat-x (coerce (car position) 'double-float))
          (dfloat-y (coerce (cdr position) 'double-float)))
      (setf (chipmunk:position rigid-body) (chipmunk:make-cp-vect dfloat-x dfloat-y)))
    player))
