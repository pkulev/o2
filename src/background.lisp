(in-package :o2)

(defclass background (game-object transform)
  ((render-priority :initform -5
                    :reader render-priority
                    :allocation :class)))
