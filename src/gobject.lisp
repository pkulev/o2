(in-package :o2)

(defclass game-object ()
  ((render-priority :initform 0
                    :accessor render-priority
                    :allocation :class)
   (x :initform 0
      :initarg :x
      :accessor x)
   (y :initform 0
      :initarg :y
      :accessor y)
   (sprite :initform nil
           :initarg :sprite
           :reader sprite)

   ;; Physics
   (dx :initform 0
       :accessor dx)
   (dy :initform 0
       :accessor dy)
   (ax :initform 0
       :accessor ax)
   (ay :initform 0
       :accessor ay)
   (max-dx :initform 20
           :reader max-dx)
   (max-dy :initform 20
           :reader max-dy)
   (max-ax :initform 5
           :reader max-ax)
   (max-ay :initform 5
           :reader max-ay)

   (fire? :initform nil
          :accessor fire?)))

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


(defclass james (game-object)
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

   (collider :initform nil
             :accessor collider)
   
   (move-speed-v :initform 20
                 :reader move-speed-v)
   (move-speed-h :initform 15
                 :reader move-speed-h)
   (move-direction-v :initform 0
                     :accessor move-direction-v)
   (move-direction-h :initform 0
                     :accessor move-direction-h)
   (pos-direction :initform 1
                  :accessor pos-direction)))

(defmethod update ((player james) &key (dt 1) &allow-other-keys)
  (with-slots (x y dx dy max-dx max-dy
               ax ay max-ax max-ay
               move-direction-h
               weapon weapons fire?) player
    ;; horizontal movement
    (when (not (zerop ax))
      (when (> (setf dx (+ dx ax)) max-dx)
        (setf dx max-dx))
      (setf x (+ x (* dx move-direction-h dt))))
      ;;(setf move-direction-h 0))
    ;; TODO: vertical movement

    ;; TODO: check screen boundaries?

    ;; TODO: update subobjects
    (dolist (weapon weapons)
      (update weapon))

    (when fire?
      ;;(make-shot weapon x y barrel-x barrel-y))))
      (format t "firing primary caliber!")
      (setf fire? nil))))

(defmethod render ((player james))
  (with-slots (x y sprite pos-direction) player
    (let ((flip (if (= pos-direction 1) :none :horizontal)))
      (when sprite (draw-sprite sprite x y :flip flip)))))

(defmethod move-left ((player james))
  (with-slots (ax max-ax move-direction-h pos-direction) player
    (setf ax max-ax)
    (setf move-direction-h -1)
    (setf pos-direction -1)))

(defmethod move-right ((player james))
  (with-slots (ax max-ax move-direction-h pos-direction) player
    (setf ax max-ax)
    (setf move-direction-h 1)
    (setf pos-direction 1)))

(defmethod jump ((player james))
  (setf (slot-value player 'move-direction-v) 1))

(defmethod fire ((player james))
  (setf (slot-value player 'fire?) t))

