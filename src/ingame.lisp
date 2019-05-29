(in-package :o2)

(defclass ingame-state (state)
  ((running :initform nil
            :accessor running?)
   (space :initform nil)))

(defmethod init ((ingame ingame-state))
  (with-slots (actor running space) ingame
    (setf running t)

    (let ((new-space (chipmunk:make-space))
          ;; Y is >0 because in SDL Y goes from top to bottom
          (gravity (chipmunk:make-cp-vect 0d0 (* 9.81d0 100))))
      (setf (chipmunk:gravity new-space) gravity)
      (setf space new-space))

    (init-collisions space)

    ;; FIXME: a better place for this?
    (let ((ground (chipmunk:make-segment-shape (chipmunk:body space)
                                               (chipmunk:make-cp-vect -1280d0 550d0)
                                               (chipmunk:make-cp-vect 1280d0 550d0)
                                               0d0)))
      (setf (chipmunk:collision-type ground) :ground)
      (setf (chipmunk:shape-filter ground) (chipmunk:make-shape-filter '(:ground) :all))
      (setf (chipmunk:friction ground) 1d0)
      (chipmunk:add space ground))

    ;; player
    (setf actor (make-james
                 :position '(50 . 400)
                 :sprite :player
                 :sitting-sprite :player-sitting
                 :space space))

    (add-object ingame
                (make-game-object
                    :components ((transform-c :position (cons -512 -385))
                                 (render-c :sprite :background :render-priority 1))
                    :systems (render-system)))

    (add-object ingame actor)

    ;; Enemy spawner
    (add-object ingame
                (make-game-object
                    :components (transform-c enemy-spawner-c)
                    :systems (enemy-spawner-system)))

    (add-object ingame (make-game-object
                        :components (transform-c
                                     (render-c :render-priority 2)
                                     (text-widget-c
                                      :data-getter
                                      (lambda ()
                                        (format nil "Score: ~A"
                                                (score (current-app-state))))))
                        :systems (text-widget-system)))


    (add-object ingame (make-game-object :components (transform-c camera-tag)
                                         :systems (camera-system)))

    ;;(add-object ingame (make-instance 'text-widget
    ;;                                  :x 0 :y 0
    ;;                                  :data-getter
    ;;                                  #'))
    ))

(defun init-collisions (space)
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

  (chipmunk:define-collision-begin-callback bullet/health-collision (arbiter space data)
    (declare (ignorable space data))

    (multiple-value-bind (bullet-shape ent-shape) (chipmunk:shapes arbiter)
      (let* ((bullet-id (cffi:pointer-address (autowrap:ptr (chipmunk:user-data bullet-shape))))
             (bullet-obj (find-object-by-id (current-app-state) bullet-id))
             (ent-id (cffi:pointer-address (autowrap:ptr (chipmunk:user-data ent-shape))))
             (ent-obj (find-object-by-id (current-app-state) ent-id)))
        (when bullet-obj
          (destroy bullet-obj)

          (when ent-obj
            (let* ((invinc-c (find-component ent-obj 'invincibility-c :raise-error nil))
                   (health-c (find-component ent-obj 'health-c))
                   (damage-range
                     (damage-range
                      (charge-type
                       (find-component bullet-obj 'weapon-charge-c))))
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
  (chipmunk:with-collision-handler-for (handler (space :player-bullet :enemy))
    (setf (chipmunk:begin-collision-fun handler) 'bullet/health-collision))
  (chipmunk:with-collision-handler-for (handler (space :enemy-bullet :player))
    (setf (chipmunk:begin-collision-fun handler) 'bullet/health-collision)))

(defmethod update ((state ingame-state) &key dt &allow-other-keys)
  (declare (ignorable dt))

  (with-slots (space) state
    ;; Chipmunk counts time in seconds, SDL - in milliseconds
    (chipmunk:step space (coerce (/ +delay+ 1000) 'double-float))

    (when (not (null *physical-component-cleanups*))
      ;; Run physics components cleanups
      (dolist (cleanup *physical-component-cleanups*)
        (funcall cleanup))
      (setf *physical-component-cleanups* '())))

  (when (next-method-p) (call-next-method)))
