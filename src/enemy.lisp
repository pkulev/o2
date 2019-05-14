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

(defclass enemy (game-object physical)
  ((render-priority :initform 1
                    :allocation :class)
   (health :initform 100
           :reader health)
   (max-health :initform 100
               :reader max-health)
   (armor :initform 0
          :reader armor)
   (weapon :initform nil)

   (x-max-velocity :initform 5)

   (pos-direction :initform 1
                  :accessor pos-direction)))

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
                           (make-instance 'health-system))))
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


(defmethod update :before ((en enemy) &key (dt 1) &allow-other-keys)
  (with-slots (actor camera) (current-app-state)
    (with-slots ((player-x x) (player-y y)) actor
      (with-slots (x x-accel x-move-direction pos-direction weapon sprite y) en
        (let* ((player-x-w/-camera (+ player-x (car camera)))
               (player-distance (abs (- player-x-w/-camera x))))
          (if (< player-distance 300)
              ;; If the enemy is near the player, stop and shoot
              (progn
                (setf x-accel 0)
                (make-shot weapon
                           (sprite-width sprite)
                           (round (/ (sprite-height sprite) 3))))
              ;; Otherwise, move towards the player
              (progn
                (setf x-move-direction (if (> player-x-w/-camera x) 1 -1))
                (setf pos-direction x-move-direction)
                (setf x-accel 1))))))))

(defmethod hurt ((en enemy) (ch weapon-charge-type))
  (with-slots (health) en
    (with-slots (damage-range) ch
      ;; (+ lower-bound (random upper-bound)) is how you do random in CL
      (let ((damage (+ (car damage-range) (random (cdr damage-range)))))
        (setf health (- health damage))
        (when (<= health 0)
          (destroy en)
          (incf (slot-value (current-app-state) 'score)))))))

;; FIXME: this is just a copy of the player's implementation,
;;        make someone that has weapons a separate trait
(defmethod select-weapon ((en enemy) weapon-class-name)
  (with-slots (children weapon) en
    (let ((child-weapon (find-if
                         (lambda (child) (typep child weapon-class-name))
                         children)))
      (if child-weapon
          (setf weapon child-weapon)
          (error (format nil "Weapon ~A isn't a child of the enemy" weapon-class-name))))))
