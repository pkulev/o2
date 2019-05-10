(in-package :o2)

(defclass weapon-charge-type ()
  ((radius :initarg :radius
           :reader radius)
   (damage-range :initarg :damage-range
                 :reader damage-range)
   (starting-velocity :initarg :starting-velocity
                      :reader starting-velocity)
   (sprite :initarg :sprite
           :reader sprite)
   (mass :initarg :mass
         :reader mass))
  (:default-initargs :radius 1 :damage-range '(0 . 0) :starting-velocity '(0d0 . 0d0)
                     :mass 0.1d0))

(defclass weapon-charge-c (component)
  ((type :initarg :charge-type)
   (collision-type :initarg :collision-type)))

(defun make-charge-object (charge-type collision-type collision-category position)
  (with-accessors ((sprite-name sprite) (mass mass)
                   (velocity starting-velocity))
      charge-type
    (with-slots (space) (current-app-state)
      (let* ((sprite-width (coerce (sprite-width sprite-name) 'double-float))
             (sprite-height (coerce (sprite-height sprite-name) 'double-float))
             (moment (chipmunk:moment-for-box mass sprite-width sprite-height))
             (body (chipmunk:add space (chipmunk:make-body mass moment)))
             (shape (chipmunk:add space (chipmunk:make-box-shape body sprite-width sprite-height 0d0))))

        (setf (chipmunk:friction shape) 0.5d0)
        (setf (chipmunk:collision-type shape) collision-type)
        (destructuring-bind (x . y) velocity
          (setf (chipmunk:velocity body) (chipmunk:make-cp-vect x y)))
        (destructuring-bind (x . y) position
          (setf (chipmunk:position body) (chipmunk:make-cp-vect
                                          (coerce x 'double-float)
                                          (coerce y 'double-float))))

        (let* ((charge-obj (make-instance
                            'game-object
                            :components (list
                                         (make-instance 'transform-c)
                                         (make-instance 'physical-c :shape shape
                                                                    :rigid-body body)
                                         (make-instance 'render-c :sprite sprite-name
                                                                  :render-priority 2)
                                         (make-instance 'weapon-charge-c :charge-type charge-type
                                                                         :collision-type collision-type))
                            :systems (list
                                      (make-instance 'physical-system)
                                      (make-instance 'render-system))))
               (obj-id (id charge-obj)))
          ;; TODO: set collision category to collide with enemies

          ;; It's a void* pointer, so just make what equals to intptr and store the id there
          (setf (chipmunk:user-data shape) (cffi:make-pointer obj-id))

          charge-obj)))))

(defmethod update ((ch weapon-charge-type) &key dt &allow-other-keys)
  (with-slots (objects camera) (current-app-state)
    (with-slots (x y shooter sprite) ch
      (etypecase shooter
        (james
         (let ((charge-rect (sdl2:make-rect (+ x (car camera)) y (sprite-width sprite) (sprite-height sprite))))
           (loop named target-iteration
                 for en in objects
                 when (typep en 'enemy)
                   do (with-slots ((en-x x) (en-y y) (en-sprite sprite)) en
                        (let ((en-rect (sdl2:make-rect
                                        en-x en-y
                                        (sprite-width en-sprite) (sprite-height en-sprite))))
                          (when (sdl2:has-intersect charge-rect en-rect)
                            (hurt en ch)
                            (destroy ch)
                            (return-from target-iteration)))))))
        (enemy
         (with-slots ((player actor)) (current-app-state)
           (let ((player-rect (get-rect player))
                 (charge-rect (sdl2:make-rect x y (sprite-width sprite) (sprite-height sprite))))
             (when (sdl2:has-intersect charge-rect player-rect)
               (hurt player ch)
               (destroy ch))))))

      ;; Delete bullets when it's not on the screen anymore
      (when (> x 1100)
        (destroy ch)))))

(defmethod render ((ch weapon-charge-type))
  (with-slots (x y sprite) ch
    (when sprite (draw-sprite sprite x y))))

(defparameter *9x19*
  (make-instance 'weapon-charge-type
                 :damage-range '(25 . 35)
                 :starting-velocity '(100d0 . 0d0)
                 :sprite :9x19))

(defclass 9x19 (weapon-charge-type) ()
  (:default-initargs :damage-range '(25 . 35)
                     :starting-velocity '(35 . 0)
                     :sprite :9x19))

(defclass 5.56x45 (weapon-charge-type)
  ((damage-range :initform (cons 35 40))))

(defclass 7.62x39 (weapon-charge-type)
  ((damage-range :initform (cons 45 50))))

(defclass 7.62x54 (weapon-charge-type)
  ((damage-range :initform (cons 99 120))))

(defclass 12x70 (weapon-charge-type)
  ((damage-range :initform (cons 35 60))
   (radius :initform 50)))

(defclass 94mm-AT (weapon-charge-type)
  ((damage-range :initform (cons 1000 1500))
   (radius :initform 500)))

(defclass M67-grenade (weapon-charge-type)
  ((damage-range :initform (cons 90 150))
   (radius :initform 250)))
