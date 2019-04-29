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
    (if parent
        (with-slots (camera) (current-app-state)
          (with-slots ((parent-x x)
                       (parent-y y)) parent

            ;; TODO: refactor-camera: remove james-specific check
            ;;       james is camera itself, so we just use its coords as offset
            (if (typep parent 'james)
                (draw-text :ubuntu (funcall data-getter)
                           (+ x parent-x)
                           (+ y parent-y)
                           :color '(255 255 255))
                (draw-text :ubuntu (funcall data-getter)
                           (+ x parent-x (- (car camera)))
                           (+ y parent-y (- (cdr camera))) :color '(255 255 255))))
 
          (draw-text :ubuntu (funcall data-getter) x y :color '(255 255 255))))))
  

