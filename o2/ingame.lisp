(in-package :o2)

(defclass ingame-state (o2/engine:state)
  ())

(defmethod o2/engine:init ((ingame ingame-state))
  (with-accessors ((actor o2/engine:actor)
                   (running? o2/engine:running?)
                   (physical-space o2/engine:physical-space)) ingame
    (setf running? t)

    (let ((new-space (chipmunk:make-space))
          ;; Y is >0 because in SDL Y goes from top to bottom
          (gravity (chipmunk:make-cp-vect 0d0 (* 9.81d0 100))))
      (setf (chipmunk:gravity new-space) gravity)
      (setf physical-space new-space))

    (init-collisions physical-space)

    ;; FIXME: a better place for this?
    (let ((ground (chipmunk:make-segment-shape (chipmunk:body physical-space)
                                               (chipmunk:make-cp-vect -1280d0 550d0)
                                               (chipmunk:make-cp-vect 1280d0 550d0)
                                               0d0)))
      (setf (chipmunk:collision-type ground) :ground)
      (setf (chipmunk:shape-filter ground) (chipmunk:make-shape-filter '(:ground) :all))
      (setf (chipmunk:friction ground) 1d0)
      (chipmunk:add physical-space ground))

    ;; player
    (setf actor (make-james
                 :position '(50 . 400)
                 :sprite :player
                 :sitting-sprite :player-sitting
                 :physical-space physical-space))

    (o2/engine:add-object ingame
                          (o2/engine:make-game-object
                           :components ((o2/engine:transform-c :position (cons -512 -385))
                                        (o2/engine:render-c :sprite :background :render-priority 1))
                           :systems (o2/engine:render-system)))

    (o2/engine:add-object ingame actor)

    ;; Enemy spawner
    (o2/engine:add-object ingame
                          (o2/engine:make-game-object
                           :components (o2/engine:transform-c enemy-spawner-c)
                           :systems (enemy-spawner-system)))

    (o2/engine:add-object ingame (o2/engine:make-game-object
                                  :components (o2/engine:transform-c
                                               (o2/engine:render-c :render-priority 2)
                                               (text-widget-c
                                                :data-getter
                                                (lambda ()
                                                  (format nil "Score: ~A"
                                                          (o2/engine:score (o2/engine:current-app-state))))))
                                  :systems (text-widget-system)))


    (o2/engine:add-object ingame (o2/engine:make-game-object
                                  :components (o2/engine:transform-c o2/engine:camera-tag)
                                  :systems (o2/engine:camera-system)))

    ;;(add-object ingame (make-instance 'text-widget
    ;;                                  :x 0 :y 0
    ;;                                  :data-getter
    ;;                                  #'))
    ))

(defun init-collisions (physical-space)
  (chipmunk:clear-collision-types)

  (chipmunk:register-collision-type :player)
  (chipmunk:register-collision-type :enemy)
  (chipmunk:register-collision-type :ground)

  ;; Bullets the player shoots
  (chipmunk:register-collision-type :player-bullet)

  ;; Bullets the enemies shoot
  (chipmunk:register-collision-type :enemy-bullet)

  (chipmunk:clear-shape-filter-categories)
  ;; Things that should not collide the player (incl. bullets)
  (chipmunk:register-shape-filter-category :player)
  (chipmunk:register-shape-filter-category :player-bullet)
  ;; Things that should not collide the enemy, and the player bullets should
  (chipmunk:register-shape-filter-category :enemy)
  (chipmunk:register-shape-filter-category :enemy-bullet)
  ;; The ground, both players and enemies should collide with it
  (chipmunk:register-shape-filter-category :ground)

  (chipmunk:define-collision-begin-callback bullet/health-collision (arbiter physical-space data)
    (declare (ignorable physical-space data))

    (multiple-value-bind (bullet-shape ent-shape) (chipmunk:shapes arbiter)
      (let* ((bullet-id (cffi:pointer-address (autowrap:ptr (chipmunk:user-data bullet-shape))))
             (bullet-obj (o2/engine:find-object-by-id (o2/engine:current-app-state) bullet-id))
             (ent-id (cffi:pointer-address (autowrap:ptr (chipmunk:user-data ent-shape))))
             (ent-obj (o2/engine:find-object-by-id (o2/engine:current-app-state) ent-id)))
        (when bullet-obj
          (o2/engine:destroy bullet-obj)

          (when ent-obj
            (let* ((invinc-c (o2/engine:find-component ent-obj 'invincibility-c :raise-error nil))
                   (health-c (o2/engine:find-component ent-obj 'health-c))
                   (damage-range
                     (damage-range
                      (charge-type
                       (o2/engine:find-component bullet-obj 'weapon-charge-c))))
                   (damage (+ (car damage-range) (random (cdr damage-range)))))

              (block hurting-block
                ;; When there is an invincible-c, the system will also check
                ;; if the entity is invincible before trying to hurt it
                (when invinc-c
                  (with-accessors ((hit-time last-hit-time)) invinc-c
                    ;; If the the entity is not invincible, the last-hit-time will be updated, otherwise
                    ;; return from the surrouding block, preventing damage
                    (if (not (is-invincible invinc-c))
                        (setf hit-time (local-time:now))
                        (return-from hurting-block))))

                (with-accessors ((health health)) health-c
                  (setf health (- health damage)))))))))

    1)
  (chipmunk:with-collision-handler-for (handler (physical-space :player-bullet :enemy))
    (setf (chipmunk:begin-collision-fun handler) 'bullet/health-collision))
  (chipmunk:with-collision-handler-for (handler (physical-space :enemy-bullet :player))
    (setf (chipmunk:begin-collision-fun handler) 'bullet/health-collision)))

(defmethod o2/engine:update ((state ingame-state) &key dt &allow-other-keys)
  (declare (ignorable dt))

  (with-accessors ((physical-space o2/engine:physical-space)) state
    ;; Chipmunk counts time in seconds, SDL - in milliseconds
    (chipmunk:step physical-space (coerce (/ o2/engine:+delay+ 1000) 'double-float))

    (when (not (null o2/engine:*physical-component-cleanups*))
      ;; Run physics components cleanups
      (dolist (cleanup o2/engine:*physical-component-cleanups*)
        (funcall cleanup))
      (setf o2/engine:*physical-component-cleanups* '())))

  (when (next-method-p) (call-next-method)))
