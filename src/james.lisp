(in-package :o2)

(defclass james (game-object)
  ((render-priority :initform 2
                    :allocation :class)
   (health :initform 100
           :reader health)
   (max-health :initform 100
               :reader max-health)
   (armor :initform 100
          :reader armor)

   (weapon :initform nil
           :accessor weapon)

   (fire? :initform nil
          :accessor fire?)

   (collider :initform nil
             :accessor collider)

   ;; Jumping
   (jumping? :initform nil
             :accessor jumping?)
   ;; Velocity added when jumping
   (jumping-added-velocity :initform -10
                           :accessor jumping-added-velocity)

   ;; Position/Rotation of the sprite
   (pos-direction :initform 1
                  :accessor pos-direction)

   (sitting? :initform nil
             :accessor sitting?)
   (sitting-sprite :initform nil
                   :initarg :sitting-sprite
                   :accessor sitting-sprite)

   ;; unix-to-timestamp 0 is definetly less than any other time
   (last-hit-time :initform (local-time:unix-to-timestamp 0)
                  :accessor last-hit-time)

   ;; amount of seconds when that the player is invincible to enemy damage
   (invinc-seconds :initform 2
                   :accessor invinc-second)))

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
                  'james
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

(defmethod select-weapon ((player james) weapon-class-name)
  (let* ((tr (find-component player 'transform-c))
         (children (children tr)))
    (with-slots (weapon) player
      (let ((child-weapon (find-if
                           (lambda (child) (typep child weapon-class-name))
                           children)))
        (if child-weapon
            (setf weapon child-weapon)
            (error (format nil "Weapon ~A isn't a child of the player" weapon-class-name)))))))

(defmethod is-invincible ((player james))
  (with-slots (last-hit-time invinc-seconds) player
    (not (local-time:timestamp> (local-time:now) (local-time:timestamp+ last-hit-time invinc-seconds :sec)))))

(defmethod hurt ((player james) (ch weapon-charge-type))
  (with-slots (health last-hit-time invinc-seconds) player
    (if (not (is-invincible player))
        (progn
          (setf last-hit-time (local-time:now))
          (with-slots (damage-range) ch
            (let ((damage (+ (car damage-range) (random (cdr damage-range)))))
              (setf health (- health damage))
              (if (<= health 0)
                  ;; Restart the game
                  ;; FIXME: this is kinda ugly
                  (push-event :restart-current-state))))))))
