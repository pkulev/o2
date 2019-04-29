(in-package :o2)

(defclass enemy-spawner (game-object)
  ((last-enemy-spawned :initform (local-time:unix-to-timestamp 0))))

(defmethod update :before ((spawner enemy-spawner) &key &allow-other-keys)
  (with-slots (children last-enemy-spawned) spawner
    ;; Allow up to 5 enemies alive, spawn them every five seconds if there's less
    (when (and (< (length children) 5)
               (local-time:timestamp> (local-time:now)
                                      (local-time:timestamp+ last-enemy-spawned 5 :sec)))
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
                                   (make-enemy :x spawn-x :y 400
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

(defun make-enemy (&key x y sprite)
  (let ((enemy (make-instance 'enemy
                              :x x :y y
                              :sprite sprite)))
    (add-child enemy
               (make-instance 'text-widget
                              :x 0 :y 0
                              :data-getter
                              #'(lambda ()
                                  (format nil "~A/~A" (health enemy) (max-health enemy)))))
    enemy))
                                  

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
               (enemy-rect (sdl2:make-rect x y
                                           (sprite-width :bad-guy)
                                           (sprite-height :bad-guy))))

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

(defmethod render ((en enemy))
  (with-slots (camera) (current-app-state)
    (with-slots (x y sprite children x-move-direction) en
      (let ((flip (if (< x-move-direction 0) :none :horizontal)))
        (when sprite (draw-sprite sprite (- x (car camera)) (- y (cdr camera)) :flip flip)
              (dolist (child children) (render child)))))))

(defmethod hurt ((en enemy) (ch weapon-charge))
  (with-slots (health) en
    (with-slots (damage-range) ch
      ;; (+ lower-bound (random upper-bound)) is how you do random in CL
      (let ((damage (+ (car damage-range) (random (cdr damage-range)))))
        (setf health (- health damage))
        (when (< health 0)
          (destroy en)
          (incf (slot-value (current-app-state) 'score)))))))
