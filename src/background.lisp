(in-package :o2)

(defclass background (game-object transform)
  ((render-priority :initform -5
                    :reader render-priority
                    :allocation :class)))

(defmethod render ((bg background))
  (with-slots (x y sprite) bg
    (with-slots (camera) (current-app-state)
      (let ((modified-x (- x (values (floor (car camera) 100))))
            (modified-y (- y (values (floor (cdr camera) 100)))))
        (draw-sprite sprite modified-x modified-y)))))
