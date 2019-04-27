(in-package :o2)

(defclass weapon-charge (physical game-object)
  ((radius :initform 1
           :reader radius)
   (damage :initform (cons 0 0)
           :reader damage)))

;;(defmethod update ((ch weapon-charge) &key dt &allow-other-keys)
;;  (format t "Actually updating charge."))

;;(defmethod render ((ch weapon-charge))
;;  (format t "Actually rendering charge."))

(defclass 9x19 (weapon-charge)
  ((damage :initform (cons 25 35))
   (x-velocity :initform 35)
   (x-accel :initform 5)
   (x-max-accel :initform 5)
   (x-max-velocity :initform 35)
   (x-move-direction :initform 1
                     :initarg :x-move-direction)))

(defclass 5.56x45 (weapon-charge)
  ((damage :initform (cons 35 40))))

(defclass 7.62x39 (weapon-charge)
  ((damage :initform (cons 45 50))))

(defclass 7.62x54 (weapon-charge)
  ((damage :initform (cons 99 120))))

(defclass 12x70 (weapon-charge)
  ((damage :initform (cons 35 60))
   (radius :initform 50)))

(defclass 94mm-AT (weapon-charge)
  ((damage :initform (cons 1000 1500))
   (radius :initform 500)))

(defclass M67-grenade (weapon-charge)
  ((damage :initform (cons 90 150))
   (radius :initform 250)))
