(in-package :o2)

(defclass weapon (o2/engine:game-object)
  ;; FIXME: Move the sprite to a component
  ((sprite :initform nil
           :initarg :sprite)
   (ammo-in-mag :initform 0
                :initarg :ammo-in-mag
                :accessor ammo-in-mag)
   (mag-capacity :initform 0
                 :initarg :mag-capacity
                 :accessor mag-capacity)
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
                     :reader current-cooldown)
   (chambered? :initform nil
               :initarg :chambered?
               :accessor chambered?)))

(defmethod cooled-down? ((w weapon))
  "Return whether weapon cooldown finished."

  (with-accessors ((last-shot last-shot)
                   (cooldown cooldown)) w

    (local-time:timestamp> (local-time:now)
                           (local-time:timestamp+ last-shot cooldown :nsec))))

(defmethod ready? ((w weapon))
  "Return if weapon ready to send out some gifts by pulling trigger."

  (with-accessors ((cooled-down? cooled-down?)
                   (chambered? chambered?)) w

    (format t "cooled: ~a, chambered ~a, ammo: ~a/~a ~%" cooled-down? chambered?
            (ammo-in-mag w) (ammo w))
    (and cooled-down? chambered?)))

;; TODO: implement some sort of pop-up with notice to user
(defmethod click-clack ((w weapon))
  "Current mag is empty."

  (format t "I've heard you're empty. Too bad!~%"))

(defmethod reload ((w weapon))
  "Reload the weapon.

  Take amount of cartriges we need to fill magazine, or what we have.
  Then load mag and substract the amount we loaded from all ammo we have."

  (format t "reloading~%")
  (with-accessors ((ammo ammo)
                   (ammo-in-mag ammo-in-mag)
                   (mag-capacity mag-capacity)) w

    (when (> ammo 0)
      (let* ((ammo-to-load (- mag-capacity ammo-in-mag))
             (amount (if (> ammo-to-load ammo) ammo ammo-to-load)))

        (setf ammo-in-mag (+ ammo-in-mag amount))
        (setf ammo (- ammo amount))))))


(defmethod o2/engine:run-system ((system o2/engine:shooter-system) found-components)
  (destructuring-bind (shoot-comp tr render-c) found-components
    (with-accessors ((shoot? o2/engine:shoot?)
                     (reload? o2/engine:reload?)
                     (weapons o2/engine:weapons)
                     (current-weapon o2/engine:current-weapon)
                     (bullet-collision-type o2/engine:bullet-collision-type)
                     (bullet-shape-filter o2/engine:bullet-shape-filter))
        shoot-comp

      (when reload?
        ;; TODO: add ability to abort reloading by taking damage
        (setf reload? nil)

        ;; TODO: fix this copy-paste
        (let ((the-weapon (find-if (lambda (w) (typep w current-weapon)) weapons)))
          (when (null the-weapon)
            ;; TODO: add a way for components to link to the object they're attached to
            (error "Weapon ~A not found" current-weapon))

          (reload the-weapon)))

      (when shoot?
        ;; The shot is actually happening, reset the "shoot?" value
        (setf shoot? nil)

        (let ((the-weapon (find-if (lambda (w) (typep w current-weapon)) weapons)))
          (when (null the-weapon)
            ;; TODO: add a way for components to link to the object they're attached to
            (error "Weapon ~A not found" current-weapon))

          (with-accessors ((cooldown cooldown)
                           (last-shot last-shot)
                           (chambered? chambered?)
                           (ready? ready?)
                           (ammo-in-mag ammo-in-mag)) the-weapon
            (when ready?
              ;; When weapon is ready, update the last time shot and shoot
              (setf last-shot (local-time:now))
              ;; Charge already gone
              (setf chambered? nil)
              (if (zerop ammo-in-mag)
                  (click-clack the-weapon)
                  (progn
                    (setf chambered? t)
                    (setf ammo-in-mag (1- ammo-in-mag))))

              (with-accessors ((par o2/engine:parent)) tr
                (with-accessors ((flip o2/engine:flip)
                                 (sprite o2/engine:sprite)) render-c
                  (let* ((pos (o2/engine:position tr))
                         (spawn-pos
                           (cons
                            (ecase flip
                              (:none (+ (o2/engine:sprite-width sprite) (car pos)))
                              (:horizontal (car pos)))
                            (cdr pos))))

                    (o2/engine:add-object (o2/engine:current-app-state)
                                          (make-charge-object (current-charge the-weapon)
                                                              bullet-collision-type
                                                              bullet-shape-filter
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
