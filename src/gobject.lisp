(in-package :o2)

(defclass jump ()
  ((jumping? :initform nil
             :accessor jumping?)
   (jump-velocity :initform -10
                  :accessor jump-velocity)))

(defclass transform ()
  ((x :initform 0
      :initarg :x
      :accessor x)
   (y :initform 0
      :initarg :y
      :accessor y)

   (x-velocity :initform 0
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

(defclass game-object ()
  ((render-priority :initform 0
                    :accessor render-priority
                    :allocation :class)
   (sprite :initform nil
           :initarg :sprite
           :reader sprite)))

(defgeneric update (object &key &allow-other-keys)
  (:documentation "Update game object state."))

(defmethod update ((object game-object) &key dt &allow-other-keys)
  (declare (ignorable dt))
  (with-slots (x y sprite) object
    nil))

(defmethod render ((object game-object))
  (with-slots (x y sprite) object
    (when sprite (draw-sprite sprite x y))))

(defmethod get-rect ((object game-object))
  (with-slots (x y sprite) object
    (if sprite
        (progn
          (let* ((texture (get-sprite-texture sprite))
                 (w (sdl2:texture-width texture))
                 (h (sdl2:texture-height texture)))
            (sdl2:make-rect x y w h)))
        (sdl2:make-rect x y 0 0))))


(defclass james (game-object transform jump)
  ((render-priority :initform 1
                    :allocation :class)
   (health :initform 100
           :reader health)
   (armor :initform 100
          :reader armor)
   (weapon :initform nil
           :accessor weapon)
   (weapons :initform (list)
            :reader weapons)
   (fire? :initform nil
          :accessor fire?)

   (collider :initform nil
             :accessor collider)
   
   (move-speed-v :initform 20
                 :reader move-speed-v)
   (move-speed-h :initform 15
                 :reader move-speed-h)

   ;; Position/Rotation of the sprite
   (pos-direction :initform 1
                  :accessor pos-direction)))

(defmethod update ((tr transform) &key (dt 1) &allow-other-keys)
  (with-slots (x y
               x-velocity x-max-velocity
               y-velocity y-max-velocity
               x-accel x-move-direction) tr
    ;; TODO: check screen boundaries?

    ;; horizontal movement
    (when (not (zerop x-accel))
      (when (> (setf x-velocity (+ x-velocity x-accel)) x-max-velocity)
        (setf x-velocity x-max-velocity))
      (setf x (+ x (* x-velocity x-move-direction dt))))

    ;; vertical movement / falling
    ;; TODO: proper ground collision, right now true here
    ;; means that the object is not falling, while false means it is
    (if (not (<= (+ y y-velocity) 400))
        (progn
          ;; if tr also can jump, handle the jumping part of it
          (if (typep tr 'jump)
              (with-slots (jumping?) tr
                (setf jumping? nil)))
          (setf y-velocity 0))
        (if (< y-velocity y-max-velocity)
            (setf y-velocity (+ y-velocity 1))))
    (setf y (+ y y-velocity)))

  (if (next-method-p) (call-next-method)))

(defmethod update ((player james) &key (dt 1) &allow-other-keys)
  (with-slots (weapon weapons fire?) player

    ;; TODO: update subobjects
    (dolist (weapon weapons)
      (update weapon))

    (when fire?
      ;;(make-shot weapon x y barrel-x barrel-y))))
      (format t "firing primary caliber!")
      (setf fire? nil)))

  (if (next-method-p) (call-next-method)))

(defmethod render ((player james))
  (with-slots (x y sprite pos-direction) player
    (let ((flip (if (= pos-direction 1) :none :horizontal)))
      (when sprite (draw-sprite sprite x y :flip flip)))))

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
  (with-slots (jumping? y y-velocity) player
    (if (not jumping?)
        (progn
          (setf y (- y 1))
          (setf jumping? t)
          (setf y-velocity -10)))))

(defmethod fire ((player james))
  (setf (slot-value player 'fire?) t))
