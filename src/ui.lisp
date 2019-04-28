(in-package :o2)

(defclass widget (transform game-object)
  ((render-priority :initform 10
                    :accessor render-priority
                    :allocation :class)))

(defclass text-widget (widget)
  ((data-getter :initarg :data-getter
                :accessor data-getter)))

(defmethod render ((w text-widget))
  (with-slots (x y data-getter) w
    (draw-text :ubuntu (funcall data-getter) x y :color '(255 255 255))))
  

