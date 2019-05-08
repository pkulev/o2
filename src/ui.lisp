(in-package :o2)

(defclass text-widget-c (component)
  ((data-getter :initarg :data-getter
                :accessor data-getter)
   (color :initarg :color
          :accessor color))

  (:default-initargs :color '(255 255 255)))

(defclass text-widget-system (system)
  ;; FIXME: a UI layer that always draws over other things
  ((requires :initform '(text-widget-c transform-c))))

(defmethod run-system ((system text-widget-system) components)
  (destructuring-bind (widget tr) components
    (with-accessors ((par parent) (pos position)) tr
      (let* ((static? (null par)) ;; Text is assumed to be static if it has no parent
             (global-pos (local-to-global-position par pos)))
        (with-accessors ((getter data-getter) (color color)) widget
          (let* ((camera-obj (find-with-component (current-app-state) 'camera-tag))
                 (camera-tr (find-component camera-obj 'transform-c))
                 (camera-pos (position camera-tr)))
            (draw-text :ubuntu (funcall getter)
                       (if static?
                           (car global-pos)
                           (- (car global-pos)
                              (car camera-pos)))
                       (if static?
                           (cdr global-pos)
                           (- (cdr global-pos)
                              (cdr camera-pos)))
                       :color color)))))))
