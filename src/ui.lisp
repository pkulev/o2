(in-package :o2)

(defclass widget (transform game-object)
  ((render-priority :initform 10
                    :accessor render-priority
                    :allocation :class)))

(defclass text-widget (widget)
  ((data-getter :initarg :data-getter
                :accessor data-getter)))

(defmethod render ((w text-widget))
  (with-slots (parent x y data-getter) w
    (format t "drawing '~a' in ~a:~a~&" (funcall data-getter) x y)
    (if parent
        (with-slots (camera) (current-app-state)
          (with-slots ((parent-x x)
                       (parent-y y)) parent
            (format t "parent ~a:~a" parent-x parent-y)
            (draw-text :ubuntu (funcall data-getter)
                       (+ x parent-x (- (car camera)))
                       (+ y parent-y (- (cdr camera))) :color '(255 255 255))))

        (draw-text :ubuntu (funcall data-getter) x y :color '(255 255 255)))))
  

