(in-package :o2)

(defclass weapon-charge (physical game-object)
  ((radius :initform 1
           :reader radius)
   (damage-range :initform (cons 0 0)
           :reader damage-range)))

(defmethod update ((ch weapon-charge) &key dt &allow-other-keys)
  (with-slots (objects camera) (current-app-state)
    (with-slots (x y sprite) ch
      (let ((charge-rect (sdl2:make-rect (+ x (car camera)) y (sprite-width sprite) (sprite-height sprite))))
        (loop for en in objects
              when (typep en 'enemy)
                do (with-slots ((en-x x) (en-y y) (en-sprite sprite)) en
                     (let ((en-rect (sdl2:make-rect
                                     en-x en-y
                                     (sprite-width en-sprite) (sprite-height en-sprite))))
                       (when (sdl2:has-intersect charge-rect en-rect)
                         (hurt en ch)
                         (destroy ch))))))

      ;; Delete bullets when it's not on the screen anymore
      (when (> x 1100)
        (destroy ch)))))

(defmethod render ((ch weapon-charge))
  (with-slots (x y sprite) ch
    (when sprite (draw-sprite sprite x y))))

(defclass 9x19 (weapon-charge)
  ((damage-range :initform (cons 25 35))
   (x-velocity :initform 35)
   (x-accel :initform 5)
   (x-max-accel :initform 5)
   (x-max-velocity :initform 35)
   (x-move-direction :initform 1
                     :initarg :x-move-direction)))

(defclass 5.56x45 (weapon-charge)
  ((damage-range :initform (cons 35 40))))

(defclass 7.62x39 (weapon-charge)
  ((damage-range :initform (cons 45 50))))

(defclass 7.62x54 (weapon-charge)
  ((damage-range :initform (cons 99 120))))

(defclass 12x70 (weapon-charge)
  ((damage-range :initform (cons 35 60))
   (radius :initform 50)))

(defclass 94mm-AT (weapon-charge)
  ((damage-range :initform (cons 1000 1500))
   (radius :initform 500)))

(defclass M67-grenade (weapon-charge)
  ((damage-range :initform (cons 90 150))
   (radius :initform 250)))
