(in-package :o2)

(defclass enemy-spawner-c (o2/engine:component)
  ((last-enemy-spawned :initarg :last-enemy-spawned
                       :accessor last-enemy-spawned))
  (:default-initargs :last-enemy-spawned (local-time:unix-to-timestamp 0)))

(defclass enemy-spawner-system (o2/engine:system)
  ((requires :initform '(enemy-spawner-c o2/engine:transform-c))))

(defmethod o2/engine:run-system ((system enemy-spawner-system) found-components)
  (destructuring-bind (spawner-comp tr) found-components
    (with-accessors ((last-spawned last-enemy-spawned)) spawner-comp
      (with-accessors ((children o2/engine:children)) tr
        ;; Allow up to 2 enemies alive, spawn them every five seconds if there's less
        (when (and (< (length children) 2)
                   (local-time:timestamp> (local-time:now)
                                          (local-time:timestamp+ last-spawned 5 :sec)))
          (setf last-spawned (local-time:now))

          (let* ((player (o2/engine:find-with-component (o2/engine:current-app-state) 'o2/engine:player-tag))
                 (player-x (car (o2/engine:global-position player)))
                 (spawn-x
                   ;; Randomly (50%) spawn enemy on the left or on the right of the player
                   (if (> (random 2) 0)
                       ;; On the right
                       (+ player-x 400)
                       ;; On the left
                       (- player-x 400))))
            (declare (special o2/engine:*game-object*))
            (o2/engine:add-child
             o2/engine:*game-object*
             (o2/engine:add-object (o2/engine:current-app-state)
                                   (make-enemy (cons spawn-x 400) :bad-guy)))))))))

(defclass enemy-system (o2/engine:system)
  ((requires :initform '(o2/engine:transform-c
                         o2/engine:physical-c
                         o2/engine:shooter-c
                         o2/engine:render-c))))

(defmethod o2/engine:run-system ((system enemy-system) found-components)
  (destructuring-bind (tr phys shooter rend) found-components
    (with-accessors ((actor o2/engine:actor)) (o2/engine:current-app-state)
      (with-accessors ((pos position)) tr
        ;; Get the global position from the enemy transform
        ;; and the player position from the actor (that has a transform)
        (let* ((enemy-x (car (o2/engine:global-position tr)))
               (player-x (car (o2/engine:global-position actor)))
               (player-distance (abs (- player-x enemy-x))))
          (with-accessors ((body o2/engine:rigid-body)) phys
            (let ((vel (chipmunk:velocity body)))
              (with-accessors ((flip o2/engine:flip)) rend
                (let ((expected-flip (if (> player-x enemy-x) :none :horizontal)))
                  (if (and (< player-distance 300) (equal flip expected-flip))
                      ;; If the enemy is near the player, stop and shoot
                      (progn
                        (setf (chipmunk:x vel) 0d0)

                        (with-accessors ((shoot? o2/engine:shoot?)) shooter
                          (setf shoot? t)))
                      ;; Otherwise, move towards the player
                      (progn
                        (setf flip expected-flip)
                        ;; FIXME: make enemy speed configurable
                        (setf (chipmunk:x vel) (* 100d0
                                                  (if (> player-x enemy-x) 1 -1)))))))
              (setf (chipmunk:velocity body) vel))))))))

(defun make-enemy (position sprite)
  (with-accessors ((physical-space o2/engine:physical-space)) (o2/engine:current-app-state)
    (let* ((dfloat-x (coerce (car position) 'double-float))
           (dfloat-y (coerce (cdr position) 'double-float))
           (mass 60.0d0)
           (moment (chipmunk:moment-for-box mass dfloat-x dfloat-y))
           (rigid-body (chipmunk:add physical-space (chipmunk:make-body
                                                     mass
                                                     moment)))
           (shape (chipmunk:add physical-space (chipmunk:make-box-shape
                                                rigid-body
                                                (coerce (o2/engine:sprite-width :bad-guy) 'double-float)
                                                (coerce (o2/engine:sprite-height :bad-guy) 'double-float)
                                                0d0)))
           (enemy-object (o2/engine:make-game-object
                          :components
                          (o2/engine:transform-c
                           (o2/engine:physical-c :shape shape :rigid-body rigid-body)
                           (o2/engine:render-c :sprite sprite :render-priority 2)
                           (o2/engine:shooter-c
                            :bullet-collision-type :enemy-bullet
                            :bullet-shape-filter (chipmunk:make-shape-filter
                                                  '(:enemy-bullet :enemy) '(:player))
                            :weapons (list
                                      (make-instance 'G17
                                                     :components (list
                                                                  (make-instance 'o2/engine:transform-c))
                                                     :ammo 32
                                                     :ammo-in-mag 17
                                                     :mag-capacity 17
                                                     :chambered? t
                                                     :sprite :G17
                                                     ;; Make the cooldown roughly 2 times bigger
                                                     :cooldown 600000000))
                            :current-weapon 'G17))
                          :systems
                          (o2/engine:physical-system
                           o2/engine:render-system
                           o2/engine:shooter-system health-system enemy-system)))
           (obj-id (o2/engine:id enemy-object)))
      ;; FIXME: to destroy the object, health-c needs a reference to the object itself,
      ;;        which is only available after the object's creation
      (push (make-instance 'health-c
                           :death-action (lambda ()
                                           (incf (o2/engine:score (o2/engine:current-app-state)))
                                           (o2/engine:destroy enemy-object)))
            (o2/engine:components enemy-object))

      (setf (chipmunk:friction shape) 0.5d0)
      (let ((dfloat-x (coerce (car position) 'double-float))
            (dfloat-y (coerce (cdr position) 'double-float)))
        (setf (chipmunk:position rigid-body) (chipmunk:make-cp-vect dfloat-x dfloat-y)))


      (setf (chipmunk:collision-type shape) :enemy)
      (setf (chipmunk:shape-filter shape) (chipmunk:make-shape-filter '(:enemy) '(:ground :player-bullet)))
      ;; Save the enemy objec ID as user data
      (setf (chipmunk:user-data shape) (cffi:make-pointer obj-id))

      (o2/engine:add-child
       enemy-object
       (o2/engine:add-object
        (o2/engine:current-app-state)
        (o2/engine:make-game-object
         :components
         (o2/engine:transform-c
          (o2/engine:render-c :render-priority 2)
          (text-widget-c
           :data-getter
           (lambda ()
             (let ((health-c (o2/engine:find-component enemy-object 'health-c)))
               (format nil "~A/~A" (health health-c) (max-health health-c))))))
         :systems (text-widget-system))))

      enemy-object)))
