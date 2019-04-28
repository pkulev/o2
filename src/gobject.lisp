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

(defmethod render ((en enemy))
    (with-slots (camera) (current-app-state)
    (with-slots (x y sprite children x-move-direction) en
      (let ((flip (if (< x-move-direction 0) :none :horizontal)))
        (when sprite (draw-sprite sprite (- x (car camera)) (- y (cdr camera)) :flip flip))

        (dolist (child children) (render child))))))

(defmethod hurt ((en enemy) (ch weapon-charge))
  (with-slots (health) en
    (with-slots (damage-range) ch
      ;; (+ lower-bound (random upper-bound)) is how you do random in CL
      (let ((damage (+ (car damage-range) (random (cdr damage-range)))))
        (setf health (- health damage))
        (when (< health 0)
          (destroy en)
          (incf (slot-value (current-app-state) 'score)))))))

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
    (remove-object (current-app-state) obj)))

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
