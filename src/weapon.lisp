(in-package :o2)

(defclass weapon (game-object)
  ;; FIXME: Move the sprite to a component
  ((sprite :initform nil
           :initarg :sprite)
   (current-ammo :initform 0
                 :initarg :current-ammo
                 :accessor current-ammo)
   (ammo :initform 0
         :initarg :ammo
         :accessor ammo)
   (max-ammo :initform 0
             :accessor max-ammo)
   (current-charge :initform nil
                   :initarg :current-charge
                   :accessor current-charge)
   ;; FIXME: change to milliseconds when delta-time is introduced into the codebase
   (cooldown :initform 300000000 ; nsec
             :initarg :cooldown
             :reader cooldown)
   (last-shot :initform (local-time:unix-to-timestamp 0)
              :accessor last-shot)
   (current-cooldown :initform 0
                     :reader current-cooldown)))

(defclass shooter-c (component)
  ((shoot? :accessor shoot?
           :initform nil)
   (weapons :accessor weapons
            :initarg :weapons)
   (current-weapon :accessor current-weapon
                   :initarg :current-weapon)
   (bullet-collision-type :accessor bullet-collision-type
                          :initarg :bullet-collision-type
                          :documentation "What collision type should be used for callbacks")
   (bullet-shape-filter :accessor bullet-shape-filter
                        :initarg :bullet-shape-filter
                        :documentation "What category should be used for collision checks"))
  (:default-initargs :weapons (list)))

(defclass shooter-system (system)
  ((requires :initform '(shooter-c transform-c render-c))))

(defmethod run-system ((system shooter-system) found-components)
  (destructuring-bind (shoot-comp tr render-c) found-components
    (with-accessors ((shoot? shoot?)
                     (weaps weapons)
                     (curr-weap current-weapon)
                     (coll-type bullet-collision-type)
                     (shp-filter bullet-shape-filter))
        shoot-comp

      (when shoot?
        ;; The shot is actually happening, reset the "shoot?" value
        (setf shoot? nil)

        (let ((the-weapon (find-if (lambda (w) (typep w curr-weap)) weaps)))
          (when (null the-weapon)
            ;; TODO: add a way for components to link to the object they're attached to
            (error "Weapon ~A not found" curr-weap))

          (with-accessors ((cooldown cooldown) (last-shot last-shot)) the-weapon
            (when (local-time:timestamp>
                   (local-time:now)
                   (local-time:timestamp+ last-shot
                                          cooldown
                                          :nsec))
              ;; If the cooldown already passed, update the last time shot and shoot
              (setf last-shot (local-time:now))

              (with-accessors ((par parent)) tr
                (with-accessors ((flip flip) (sprite sprite)) render-c
                  (let* ((pos (position tr))
                         (spawn-pos
                           (cons
                            (ecase flip
                              (:none (+ (sprite-width sprite) (car pos)))
                              (:horizontal (car pos)))
                            (cdr pos))))

                    (add-object (current-app-state)
                                (make-charge-object (current-charge the-weapon)
                                                    coll-type
                                                    shp-filter
                                                    spawn-pos
                                                    flip))))))))))))

(defclass G17 (weapon) ()
  (:default-initargs :current-charge *9x19*))

(defclass MP5SD (weapon)
  ((current-charge :initform *9x19*)))

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
