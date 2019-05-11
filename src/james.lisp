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
  (let* ((dfloat-x (coerce (car position) 'double-float))
         (dfloat-y (coerce (cdr position) 'double-float))
         (mass 60.0d0)
         (moment (chipmunk:moment-for-box mass dfloat-x dfloat-y))
         (rigid-body (chipmunk:add space (chipmunk:make-body
                                          mass
                                          moment)))
         (shape (chipmunk:add space (chipmunk:make-box-shape
                                     rigid-body
                                     (coerce (sprite-width :player) 'double-float)
                                     (coerce (sprite-height :player) 'double-float)
                                     0d0)))
         (player (make-instance
                  'james
                  :components (list
                               (make-instance 'player-tag)
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
                                                                    '(:player) '(:enemy))
                                              :weapons (list
                                                        (make-instance 'G17
                                                                       :components (list
                                                                                    (make-instance 'transform-c))
                                                                       :ammo 32
                                                                       :current-ammo 17
                                                                       :sprite :G17))
                                              :current-weapon 'G17))
                  :systems (list
                            (make-instance 'physical-system)
                            (make-instance 'player-controlable-system)
                            (make-instance 'render-system)
                            (make-instance 'shooter-system))

                  :sprite sprite
                  :sitting-sprite sitting-sprite)))

    (setf (chipmunk:collision-type shape) :james)
    ; (setf (chipmunk:shape-filter shape) (chipmunk:make-shape-filter '(:player) '()))

    (chipmunk:with-collision-handler-for (handler (space :james :ground))
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
                                                    (format nil "~A/~A ~A"
                                                            (health player)
                                                            (max-health player)
                                                            (if (is-invincible player) "[I]" ""))))))
                                :systems
                                (list
                                 (make-instance 'text-widget-system)))))

    (setf (chipmunk:friction shape) 0.5d0)
    (setf (chipmunk:position rigid-body) (chipmunk:make-cp-vect dfloat-x dfloat-y))

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

;; TODO: put the defgeneric for hurt somewhere
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
                  (progn
                    (deregister-state *application* :ingame)
                    (register-state *application* 'ingame-state :ingame)
                    (set-state *application* :ingame)))))))))

(defmethod update ((player james) &key (dt 1) &allow-other-keys)
  (declare (ignorable dt))

  (with-slots (weapon weapons
               fire? jumping?
               y y-velocity
               x x-velocity x-move-direction
               pos-direction sprite children) player
    ;; FIXME: 400 is the hardcoded floor

    (run-systems player)

    ;; (if (not (<= (+ y y-velocity) 400))
    ;;     (setf jumping? nil))
    ;;
    ;; ;; FIXME: screen boundry check, do something proper here
    ;; (let ((next-x (+ x (* x-move-direction x-velocity))))
    ;;   (when (<= next-x 0)
    ;;     (setf x 0))
    ;;   (when (>= next-x 412)
    ;;     (setf x 412)))
    ;;
    ;; (let* ((camera (camera (current-app-state)))
    ;;        (modified-camera-x (+ (car camera) (* x-move-direction x-velocity)))
    ;;        (modified-camera-y (+ (cdr camera) y-velocity)))
    ;;
    ;;   (when (>= modified-camera-x 0)
    ;;     (setf (car camera) modified-camera-x))
    ;;   (setf (cdr camera) modified-camera-y))
    ;;
    ;; (when fire?
    ;;   ;;(make-shot weapon x y barrel-x barrel-y))))
    ;;   (make-shot weapon
    ;;              (sprite-width sprite)
    ;;              (round (/ (sprite-height sprite) 3)))
    ;;   (setf fire? nil))

    ;; (when (next-method-p) (call-next-method))
    ))

(defmethod render ((player james))
  ;; (with-slots (sprite pos-direction sitting? sitting-sprite children) player
  ;;   (let* ((rigid-body (rigid-body (find-component player 'physical-c)))
  ;;          (pos (chipmunk:position rigid-body))
  ;;          (x (chipmunk:x pos)) (y (chipmunk:y pos))
  ;;          (flip (if (= pos-direction 1) :none :horizontal)))
  ;;     (if (not sitting?)
  ;;         (when sprite (draw-sprite sprite x y :flip flip))
  ;;         ;; FIXME: 30 is a hardcoded value of leg difference when sitting
  ;;         (when sitting-sprite (draw-sprite sitting-sprite x (+ y 30) :flip flip))))
  ;;   ;; FIXME: Not call-next-method because player doesn't need to change accroding to the camera
  ;;   (dolist (child children) (render child)))
  )

(defmethod move-left ((player james))
  (with-slots (x-accel x-max-accel x-move-direction pos-direction) player
    (setf x-accel x-max-accel)
    (setf x-move-direction -1)
    (setf pos-direction -1)))

(defmethod move-right ((player james))
  (with-slots (x-accel x-max-accel x-move-direction pos-direction) player
    (setf x-accel x-max-accel)
    (setf x-move-direction 1)
    (setf pos-direction 1)))

(defmethod jump ((player james))
  (with-slots (jumping? jumping-added-velocity y y-velocity) player
    (if (not jumping?)
        (progn
          (setf jumping? t)
          (setf y-velocity jumping-added-velocity)))))

(defmethod fire ((player james))
  (setf (slot-value player 'fire?) t))
