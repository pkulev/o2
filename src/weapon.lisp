(in-package :o2)

(defclass weapon (game-object)
  ((current-ammo :initform 0
                 :initarg :current-ammo
                 :accessor current-ammo)
   (ammo :initform 0
         :initarg :ammo
         :accessor ammo)
   (max-ammo :initform 0
             :accessor max-ammo)
   (current-charge :initform nil)
   (cooldown :initform 0
             :reader cooldown)
   (current-cooldown :initform 0
                     :reader current-cooldown)))

(defmethod make-shot ((wp weapon) x y)
  "X and Y are local coordinates to spawn the bullet at"
  (with-slots (parent) wp
    (with-slots ((parent-x x) (parent-y y) (parent-x-move-direction pos-direction)) parent

      (let ((spawn-x (+ parent-x (if (> parent-x-move-direction 0) x 0)))
            (spawn-y (+ parent-y y)))

        (let ((ch (make-instance '9x19
                                 :x spawn-x :y spawn-y
                                 :x-move-direction parent-x-move-direction
                                 :sprite :9x19)))
          (add-object (current-app-state) ch))))))

;;(defmethod update ((wp weapon) &key dt &allow-other-keys)
;;  (format t "updating weapon ~a~&" wp)

;;(defmethod render ((wp weapon))
;;  (with-slots (sprite) wp
;;    (format t "rendering weapon ~a~&" wp)

(defclass G17 (weapon)
  ((current-charge :initform (find-class '9x19))))

(defclass MP5SD (weapon)
  ((current-charge :initform (find-class '9x19))))

(defclass AKM (weapon)
  ((current-charge :initform (find-class '7.62x39))))

(defclass M16A2 (weapon)
  ((current-charge :initform (find-class '5.56x45))))

(defclass SVD (weapon)
  ((current-charge :initform (find-class '7.62x54))))

(defclass SPAS-12 (weapon)
  ((current-charge :initform (find-class '12x70))))

(defclass Jackhammer (weapon)
  ((current-charge :initform (find-class '12x70))))

(defclass M67 (weapon)
  ((current-charge :initform (find-class 'M67-grenade))))

(defclass LAW-80 (weapon)
  ((current-charge :initform (find-class '94mm-AT))))

;; reload

;; (defclass knife (weapon)
;;   )

;; (defclass g17 (weapon)
;;   ((allowed-charges :initform (list (find-class 'bullet))
;;                     :allocation :class)
;;    (default-charge :initform (list (find-clss 'bullet))
;;                    :allocation :class)
;;   ))

;; (defclass mp5sd (weapon)
;;   )

;; (defclass akm (weapon)
;;   )

;; (defclass m16a2 (weapon)
;;   )

;; (defclass m69 (weapon)
;;   )
