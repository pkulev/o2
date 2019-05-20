(in-package :o2)

(defclass enemy-spawner-c (component)
  ((last-enemy-spawned :initarg :last-enemy-spawned
                       :accessor last-enemy-spawned))
  (:default-initargs :last-enemy-spawned (local-time:unix-to-timestamp 0)))

(defclass enemy-spawner-system (system)
  ((requires :initform '(enemy-spawner-c transform-c))))

(defmethod run-system ((system enemy-spawner-system) found-components)
  (destructuring-bind (spawner-comp tr) found-components
      (with-accessors ((last-spawned last-enemy-spawned)) spawner-comp
        (with-accessors ((children children)) tr
          ;; Allow up to 2 enemies alive, spawn them every five seconds if there's less
          (when (and (< (length children) 2)
                     (local-time:timestamp> (local-time:now)
                                            (local-time:timestamp+ last-spawned 5 :sec)))
            (setf last-spawned (local-time:now))

            (let* ((player (find-with-component (current-app-state) 'player-tag))
                   (player-x (car (global-position player)))
                   (spawn-x
                     ;; Randomly (50%) spawn enemy on the left or on the right of the player
                     (if (> (random 2) 0)
                         ;; On the right
                         (+ player-x 400)
                         ;; On the left
                         (- player-x 400))))
              (add-child
               (game-object system)
               (add-object (current-app-state)
                           (make-enemy (cons spawn-x 400) :bad-guy)))))))))

(defclass enemy-system (system)
  ((requires :initform '(transform-c physical-c shooter-c render-c))))

(defmethod run-system ((system enemy-system) found-components)
  (destructuring-bind (tr phys shooter rend) found-components
    (with-slots (actor) (current-app-state)
      (with-accessors ((pos position)) tr
        ;; Get the global position from the enemy transform
        ;; and the player position from the actor (that has a transform)
        (let* ((enemy-x (car (global-position tr)))
               (player-x (car (global-position actor)))
               (player-distance (abs (- player-x enemy-x))))
          (with-accessors ((body rigid-body)) phys
            (let ((vel (chipmunk:velocity body)))
              (with-accessors ((flip flip)) rend
                (let ((expected-flip (if (> player-x enemy-x) :none :horizontal)))
                  (if (and (< player-distance 300) (equal flip expected-flip))
                      ;; If the enemy is near the player, stop and shoot
                      (progn
                        (setf (chipmunk:x vel) 0d0)

                        (with-accessors ((shoot? shoot?)) shooter
                          (setf shoot? t)))
                      ;; Otherwise, move towards the player
                      (progn
                        (setf flip expected-flip)
                        ;; FIXME: make enemy speed configurable
                        (setf (chipmunk:x vel) (* 100d0
                                                  (if (> player-x enemy-x) 1 -1)))))))
              (setf (chipmunk:velocity body) vel))))))))

(defun make-enemy (position sprite)
  (with-slots (space) (current-app-state)
    (let* ((dfloat-x (coerce (car position) 'double-float))
           (dfloat-y (coerce (cdr position) 'double-float))
           (mass 60.0d0)
           (moment (chipmunk:moment-for-box mass dfloat-x dfloat-y))
           (rigid-body (chipmunk:add space (chipmunk:make-body
                                            mass
                                            moment)))
           (shape (chipmunk:add space (chipmunk:make-box-shape
                                       rigid-body
                                       (coerce (sprite-width :bad-guy) 'double-float)
                                       (coerce (sprite-height :bad-guy) 'double-float)
                                       0d0)))
           (enemy-object (make-instance
                          'game-object
                          :components
                          (list
                           (make-instance 'physical-c :shape shape :rigid-body rigid-body)
                           (make-instance 'transform-c)
                           (make-instance 'render-c :sprite sprite :render-priority 2)
                           (make-instance 'shooter-c
                                          :bullet-collision-type :enemy-bullet
                                          :bullet-shape-filter (chipmunk:make-shape-filter
                                                                '(:enemy) '(:player))
                                          :weapons (list
                                                    (make-instance 'G17
                                                                   :components (list
                                                                                (make-instance 'transform-c))
                                                                   :ammo 32
                                                                   :current-ammo 17
                                                                   :sprite :G17))
                                          :current-weapon 'G17)
                           (make-instance 'health-c))
                          :systems
                          (list
                           (make-instance 'physical-system)
                           (make-instance 'render-system)
                           (make-instance 'shooter-system)
                           (make-instance 'health-system)
                           (make-instance 'enemy-system))))
           (obj-id (id enemy-object)))
      (setf (chipmunk:friction shape) 0.5d0)
      (let ((dfloat-x (coerce (car position) 'double-float))
            (dfloat-y (coerce (cdr position) 'double-float)))
        (setf (chipmunk:position rigid-body) (chipmunk:make-cp-vect dfloat-x dfloat-y)))


      (setf (chipmunk:collision-type shape) :enemy)
      (setf (chipmunk:shape-filter shape) (chipmunk:make-shape-filter '(:enemy) '(:ground :player-bullet)))
      ;; Save the enemy objec ID as user data
      (setf (chipmunk:user-data shape) (cffi:make-pointer obj-id))

      (add-child
       enemy-object
       (add-object
        (current-app-state)
        (make-instance 'game-object
                       :components
                       (list
                        (make-instance 'transform-c)
                        (make-instance 'render-c :render-priority 2)
                        (make-instance 'text-widget-c
                                       :data-getter
                                       (lambda ()
                                         (let ((health-c (find-component enemy-object 'health-c)))
                                           (format nil "~A/~A" (health health-c) (max-health health-c))))))
                       :systems
                       (list
                        (make-instance 'text-widget-system)))))

      enemy-object)))

#|
(defmethod hurt ((en enemy) (ch weapon-charge-type))
  (with-slots (health) en
    (with-slots (damage-range) ch
      ;; (+ lower-bound (random upper-bound)) is how you do random in CL
      (let ((damage (+ (car damage-range) (random (cdr damage-range)))))
        (setf health (- health damage))
        (when (<= health 0)
          (destroy en)
          (incf (slot-value (current-app-state) 'score)))))))
|#
