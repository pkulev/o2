(in-package :o2)

(defclass transform ()
  ((x :initform 0
      :initarg :x
      :accessor x)
   (y :initform 0
      :initarg :y
      :accessor y)))

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
