(in-package :o2-game)

(defclass text-widget-c (o2/engine:component)
  ((data-getter :initarg :data-getter
                :accessor data-getter)
   (color :initarg :color
          :accessor color))

  (:default-initargs :color '(255 255 255)))

(defclass text-widget-system (o2/engine:system)
  ;; FIXME: a UI layer that always draws over other things
  ((requires :initform '(text-widget-c o2/engine:transform-c))))

(defmethod o2/engine:run-system ((system text-widget-system) components)
  (destructuring-bind (widget tr) components
    (with-accessors ((par o2/engine:parent) (pos o2/engine:position)) tr
      (let* ((static? (null par)) ;; Text is assumed to be static if it has no parent
             (global-pos (o2/engine:local-to-global-position par pos)))
        (with-accessors ((getter data-getter) (color color)) widget
          (let* ((camera-obj (o2/engine:find-with-component (o2/engine:current-app-state) 'o2/engine:camera-tag))
                 (camera-tr (o2/engine:find-component camera-obj 'o2/engine:transform-c))
                 (camera-pos (o2/engine:position camera-tr)))
            (o2/engine:draw-text :ubuntu (funcall getter)
                                 (if static?
                                     (car global-pos)
                                     (- (car global-pos)
                                        (car camera-pos)))
                                 (if static?
                                     (cdr global-pos)
                                     (- (cdr global-pos)
                                        (cdr camera-pos)))
                                 :color color)))))))
