(in-package :o2)

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
              ;; Restart the game
              ;; FIXME: this is kinda ugly
              (progn
                (deregister-state *application* :ingame)
                (register-state *application* 'ingame-state :ingame)
                (set-state *application* :ingame)))))))

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
