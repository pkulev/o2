(in-package :o2)

(defclass physical (transform)
  ((x-velocity :initform 0
               :accessor x-velocity)
   (x-max-velocity :initform 20
                   :accessor max-x-velocity)
   (x-accel :initform 0
            :accessor x-accel)
   (x-max-accel :initform 5
                :accessor x-max-accel)

   (y-velocity :initform 0
               :accessor y-velocity)
   (y-max-velocity :initform 20
                   :accessor y-max-velocity)

   (x-move-direction :initform 0
                     :accessor x-move-direction)))

(defclass transform ()
  ((x :initform 0
      :initarg :x
      :accessor x)
   (y :initform 0
      :initarg :y
      :accessor y)))

(defclass game-object ()
  ((render-priority :initform 0
                    :accessor render-priority
                    :allocation :class)
   (sprite :initform nil
           :initarg :sprite
           :reader sprite)

   (parent :initform nil
           :reader parent)

   (children :initform (list)
             :reader children)))

(defgeneric update (object &key &allow-other-keys)
  (:documentation "Update game object state."))

(defmethod update ((object game-object) &key dt &allow-other-keys)
  (declare (ignorable dt))
  (with-slots (x y sprite) object
    nil))

(defmethod render ((object game-object))
  (with-slots (x y sprite children) object
    (with-slots (camera) (current-app-state)
      (when sprite (draw-sprite sprite (- x (car camera)) (- y (cdr camera)))))
    (dolist (child children) (render child))))

(defmethod get-rect ((object game-object))
  (with-slots (x y sprite) object
    (if sprite
        (progn
          (let* ((texture (get-sprite-texture sprite))
                 (w (sdl2:texture-width texture))
                 (h (sdl2:texture-height texture)))
            (sdl2:make-rect x y w h)))
        (sdl2:make-rect x y 0 0))))

(defmethod add-child ((object game-object) (child game-object))
  (with-slots (children) object
    (with-slots (parent) child
      (unless (find child children)
        (push child children)
        (setf parent object)))))
(defmethod remove-child ((object game-object) (child game-object))
  (with-slots (children) object
    (with-slots (parent) child
      (setf children (remove child children))
      (setf parent nil))))

(defmethod update :after ((object game-object) &key &allow-other-keys)
  (dolist (child (children object))
    (update child)))

(defclass enemy-spawner (game-object)
  ((last-enemy-spawned :initform (local-time:unix-to-timestamp 0))))

(defmethod update :before ((spawner enemy-spawner) &key &allow-other-keys)
  (with-slots (children last-enemy-spawned) spawner
    ;; Allow up to 5 enemies alive, spawn them every five seconds if there's less
    (when (and (< (length children) 5)
               (local-time:timestamp> (local-time:now) (local-time:timestamp+ last-enemy-spawned 5 :sec)))
      (setf last-enemy-spawned (local-time:now))

      (with-slots (actor camera) (current-app-state)
        (with-slots ((player-x x)) actor
          (let* ((player-x-w/-camera (+ player-x (car camera)))
                 (spawn-x
                   ;; Randomly (50%) spawn enemy on the left or on the right of the player
                   (if (> (random 2) 0)
                       ;; On the right
                       (+ player-x-w/-camera 1024)
                       ;; On the left
                       (- player-x-w/-camera 1024))))
            (add-child spawner
                       (add-object (current-app-state)
                                   (make-instance 'enemy
                                                  :x spawn-x :y 400
                                                  :sprite :bad-guy)))))))))

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

   (x-max-velocity :initform 5)))

(defmethod update :before ((en enemy) &key (dt 1) &allow-other-keys)
  (with-slots (actor camera) (current-app-state)
    (with-slots ((player-x x) (player-y y)) actor
      (with-slots (x x-accel x-move-direction y) en
        (let* ((player-x-w/-camera (+ player-x (car camera)))
               (player-y-w/-camera (+ player-y (cdr camera)))
               (player-rect (sdl2:make-rect
                             player-x-w/-camera
                             player-y-w/-camera
                             (sprite-width :player) (sprite-height :player)))
              (enemy-rect (sdl2:make-rect x y (sprite-width :bad-guy) (sprite-height :bad-guy))))

          (if (sdl2:has-intersect enemy-rect player-rect)
              ;; If the enemy touches the player, stop and damage the player
              (progn
                (setf x-move-direction 0)
                (setf x-accel 0)
                (damage-player actor))
              ;; Otherwise, move towards the player
              (progn
                (setf x-move-direction (if (> player-x-w/-camera x) 1 -1))
                (setf x-accel 1))))))))

(defmethod render :after ((en enemy))
  (with-slots (camera) (current-app-state)
    (with-slots (health max-health x y) en
      (draw-text
       :ubuntu
       (format nil "~A/~A" health max-health)
       (- x (car camera)) (- y (cdr camera))
       :color '(255 255 255)))))

(defmethod hurt ((en enemy) (ch weapon-charge))
  (with-slots (health) en
    (with-slots (damage-range) ch
      ;; (+ lower-bound (random upper-bound)) is how you do random in CL
      (let ((damage (+ (car damage-range) (random (cdr damage-range)))))
        (setf health (- health damage))
        (when (< health 0)
          (destroy en))))))

(defmethod destroy ((obj game-object))
  (with-slots (objects) (current-app-state)
    (with-slots (parent children) obj
      ;; Also destroy children
      (dolist (child children)
        (destroy child))

      ;; Disconnect from parent, if there is one.
      ;; This also removes the parent reference from the child
      (when parent
        (with-slots (children) parent
          (remove-child parent obj))))
    ;; Remove the object from the global object list
    (setf objects (remove obj objects))))

(defclass james (game-object physical)
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

   (move-speed-v :initform 20
                 :reader move-speed-v)
   (move-speed-h :initform 15
                 :reader move-speed-h)

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

(defmethod select-weapon ((player james) weapon-class-name)
  (with-slots (children weapon) player
    (let ((child-weapon (find-if
                       (lambda (child) (typep child weapon-class-name))
                       children)))
      (if child-weapon
          (setf weapon child-weapon)
          (error (format nil "Weapon ~A isn't a child of the player" weapon-class-name))))))

(defmethod is-invincible ((player james))
  (with-slots (last-hit-time invinc-seconds) player
    (not (local-time:timestamp> (local-time:now) (local-time:timestamp+ last-hit-time invinc-seconds :sec)))))

;; TODO: unify into hurt somehow
(defmethod damage-player ((player james))
  (with-slots (health last-hit-time invinc-seconds) player
    (if (not (is-invincible player))
        (progn
          (setf last-hit-time (local-time:now))
          (setf health (- health 10))
          (if (<= health 0)
              (destroy player))))))

(defmethod update :before ((tr physical) &key (dt 1) &allow-other-keys)
  (with-slots (x y
               x-velocity x-max-velocity
               y-velocity y-max-velocity
               x-accel x-move-direction) tr
    ;; TODO: check screen boundaries?

    ;; FIXME: move it somewhere, this is not a place for a james-specific thing
    (when (and (typep tr 'james) (not (zerop x-velocity)))
      (with-slots (sitting?) tr
        (when sitting?
          (setf x-velocity (values (floor x-velocity 2))))))

    ;; horizontal movement
    (when (not (zerop x-accel))
      (when (> (setf x-velocity (+ x-velocity x-accel)) x-max-velocity)
        (setf x-velocity x-max-velocity))
      (setf x (+ x (* x-velocity x-move-direction dt))))

    ;; vertical movement / falling
    ;; TODO: proper ground collision, right now true here
    ;; means that the object is not falling, while false means it is
    (if (not (<= (+ y y-velocity) 400))
        (setf y-velocity 0)
        (if (< y-velocity y-max-velocity)
            (setf y-velocity (+ y-velocity 1))))
    (setf y (+ y y-velocity))))

(defmethod update ((player james) &key (dt 1) &allow-other-keys)
  (with-slots (weapon weapons
               fire? jumping?
               y y-velocity
               x x-velocity x-move-direction
               pos-direction sprite) player
    ;; FIXME: 400 is the hardcoded floor
    (if (not (<= (+ y y-velocity) 400))
        (setf jumping? nil))

    ;; FIXME: screen boundry check, do something proper here
    (let ((next-x (+ x (* x-move-direction x-velocity))))
      (when (<= next-x 0)
        (setf x 0))
      (when (>= next-x 640)
        (setf x 640)))

    (let* ((camera (camera (current-app-state)))
           (modified-camera-x (+ (car camera) (* x-move-direction x-velocity)))
           (modified-camera-y (+ (cdr camera) y-velocity)))

      (when (>= modified-camera-x 0)
        (setf (car camera) modified-camera-x))
      (setf (cdr camera) modified-camera-y))

    (when fire?
      ;;(make-shot weapon x y barrel-x barrel-y))))
      (make-shot weapon
                 (sprite-width sprite)
                 (round (/ (sprite-height sprite) 3)))
      (setf fire? nil))))

(defmethod render ((player james))
  (with-slots (x y sprite pos-direction sitting? sitting-sprite) player
    (let ((flip (if (= pos-direction 1) :none :horizontal)))
      (if (not sitting?)
          (when sprite (draw-sprite sprite x y :flip flip))
          ;; FIXME: 30 is a hardcoded value of leg difference when sitting
          (when sitting-sprite (draw-sprite sitting-sprite x (+ y 30) :flip flip))))))

(defmethod render :after ((player james))
  (with-slots (health max-health x y) player
    (draw-text
     :ubuntu
     (format nil "~A/~A ~A" health max-health (if (is-invincible player) "[I]" ""))
     x y
     :color '(255 255 255))))

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
