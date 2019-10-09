(in-package :o2)

(defclass health-c (o2/engine:component)
  ((max-health :initarg :max-health
               :accessor max-health)
   (health :initarg :health
           :accessor health)
   (death-action :initarg :death-action
                 :accessor death-action))
  (:default-initargs :health 100 :max-health 100 :death-action nil))

(defclass invincibility-c (o2/engine:component)
  ((last-hit-time
    ;; unix-to-timestamp 0 is definetly less than any other time
    :initform (local-time:unix-to-timestamp 0)
    :accessor last-hit-time)
   (invinc-seconds :initarg :invinc-seconds
                   :accessor invinc-seconds))
  (:default-initargs :invinc-seconds 2))

(defmethod is-invincible ((invinc-comp invincibility-c))
  "If the current time is more than the time that needs to pass for invincibility to work,
   the entity is not invincible"
  (with-accessors ((hit-time last-hit-time) (invinc-secs invinc-seconds)) invinc-comp
    (not (local-time:timestamp> (local-time:now) (local-time:timestamp+ hit-time invinc-secs :sec)))))

(defclass health-system (o2/engine:system)
  ((requires :initform '(health-c))))

(defmethod o2/engine:run-system ((system health-system) found-components)
  (destructuring-bind (h-comp) found-components
    (with-accessors ((obj game-object)) system
      (with-accessors ((health health) (d-action death-action)) h-comp
        (when (<= health 0)
          (when d-action
            (funcall d-action)))))))
