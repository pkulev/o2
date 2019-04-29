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
   ;; FIXME: change to milliseconds when delta-time is introduced into the codebase
   (cooldown :initform 300000000 ; nsec
             :reader cooldown)
   (last-shot :initform (local-time:unix-to-timestamp 0)
              :accessor last-shot)
   (current-cooldown :initform 0
                     :reader current-cooldown)))

(defmethod make-shot ((wp weapon) x y)
  "X and Y are local coordinates to spawn the bullet at"
  (with-slots (parent cooldown last-shot) wp
    (when (local-time:timestamp>
           (local-time:now)
           (local-time:timestamp+ last-shot
                                  ;; Enemies shoot twice as slow
                                  cooldown
                                  :nsec))
      ;; If the cooldown already passed, update the last time shot and shoot
      (setf last-shot (local-time:now))

      (with-slots ((parent-x x) (parent-y y) (parent-x-move-direction pos-direction)) parent
        (destructuring-bind
            (spawn-x spawn-y)
            ;; FIXME: this can happen aparently, find out why and fix
            (when parent
              (etypecase parent
                (james (list
                        (+ parent-x (if (> parent-x-move-direction 0) x 0))
                        (+ parent-y y)))
                (enemy (with-slots (camera) (current-app-state)
                         (list
                          (- (+ parent-x (if (> parent-x-move-direction 0) x 0)) (car camera))
                          (- (+ parent-y y) (cdr camera)))))))
          (let ((ch (make-instance '9x19
                                   :x spawn-x :y spawn-y
                                   :x-move-direction parent-x-move-direction
                                   :sprite :9x19
                                   :shooter parent)))
            (add-object (current-app-state) ch)))))))

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
