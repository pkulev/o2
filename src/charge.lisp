(in-package :o2)

(defclass weapon-charge-type ()
  ((radius :initarg :radius
           :reader radius)
   (damage-range :initarg :damage-range
                 :reader damage-range)
   (starting-velocity :initarg :starting-velocity
                      :reader starting-velocity)
   (sprite :initarg :sprite
           :reader sprite))
  (:default-initargs :radius 1 :damage-range '(0 . 0) :starting-velocity '(0d0 . 0d0)))

(defclass weapon-charge-c (component)
  ((charge-type :initarg :charge-type
                :reader charge-type)
   (collision-type :initarg :collision-type)))

(defun make-charge-object (charge-type collision-type shape-filter position flip)
  (with-accessors ((sprite-name sprite) ; (mass mass)
                   (velocity starting-velocity))
      charge-type
    (with-slots (space) (current-app-state)
      (let* ((sprite-width (coerce (sprite-width sprite-name) 'double-float))
             (sprite-height (coerce (sprite-height sprite-name) 'double-float))
             (body (chipmunk:add space (chipmunk:make-kinematic-body)))
             (shape (chipmunk:add space (chipmunk:make-box-shape body sprite-width sprite-height 0d0))))

        (setf (chipmunk:sensor shape) t)
        (setf (chipmunk:friction shape) 1d0)
        (setf (chipmunk:collision-type shape) collision-type)
        (setf (chipmunk:shape-filter shape) shape-filter)

        (destructuring-bind (x . y) velocity
          (setf (chipmunk:velocity body) (chipmunk:make-cp-vect (ecase flip
                                                                  (:none x)
                                                                  (:horizontal (- x)))
                                                                y)))
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

(defparameter *9x19*
  (make-instance 'weapon-charge-type
                 :damage-range '(25 . 35)
                 :starting-velocity '(1000d0 . 0d0)
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
