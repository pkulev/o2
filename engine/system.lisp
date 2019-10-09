(in-package :o2/engine)

(defclass system ()
  ((requires :initform '()
             :accessor requires
             :documentation
             "Components required for the system.
              A list of either component class names or
              (:designator name) for special components, currently the only one
              available is :optional")))

(defclass physical-system (system)
  ((requires :initform '(transform-c physical-c render-c))))

(defclass render-system (system)
  ((requires :initform '(render-c transform-c))))

(defclass camera-system (system)
  ((requires :initform '(transform-c))))

(defmethod run-systems (object)
  (with-accessors ((object-systems systems) (components components)) object
    (with-accessors ((system-instances systems)) (current-app-state)
      (dolist (system-class-name object-systems)

        ;; Look up the system instance in the state system map;
        ;; if it's there - use it, otherwise create a new one and put it into
        ;; the map, symbol-macrolet ensures that the instance is put into the map
        (symbol-macrolet ((system-instance (gethash system-class-name system-instances)))
          (when (null system-instance)
            (let ((new-instance (make-instance system-class-name)))
              (setf system-instance new-instance)))
          (with-accessors ((requirements requires)) system-instance
            ;; Find the required components that the system specified,
            ;; if some of them were not found, an error will be signaled by the
            ;; find-component function
            ;; TODO: have a way to handle not finding components, like a special
            ;;       signal or something
            (let ((found-required-components
                    (loop for required-comp in requirements
                          collect
                          (if (listp required-comp)
                              ;; If the component is a list, then it must be in
                              ;; the (:designator name) format and indicates
                              ;; some kind of special component. Currently, only
                              ;; :optional is valid, but that might change in
                              ;; the future.
                              (destructuring-bind (designator name) required-comp
                                (ecase designator
                                  (:optional (find-component object name
                                                             :raise-error nil))))
                              ;; Otherwise, look up the component and throw a
                              ;; error if it was not found
                              (find-component object required-comp
                                              :raise-error t)))))
              (let ((*game-object* object))
                ;; Declare *game-object* as special, so that if needed system
                ;; could access it by doing the same thing
                (declare (special *game-object*))

                (run-system system-instance found-required-components)))))))))

(defclass shooter-system (system)
  ((requires :initform '(shooter-c transform-c o2/engine:render-c))))

(defgeneric run-system (system found-required-components))

(defmethod run-system ((system physical-system) found-components)
  "Update the position of the object from the physical system"
  (destructuring-bind (transform physical render) found-components
    (with-accessors ((body rigid-body)) physical
      (with-accessors ((pos position) (parent parent)) transform
        (with-accessors ((sprite sprite)) render
          (let ((phys-pos (chipmunk:position body)))
            (setf pos
                  ;; Translate the global position from the physics system into
                  ;; a local position, if there is a parent.
                  (global-to-local-position
                   parent
                   (cons (- (round (chipmunk:x phys-pos))
                            (floor (sprite-width sprite) 2))
                         ;; FIXME: Y should actually be +ed with half of the
                         ;;        sprite's height, but it doesn't look
                         ;;        quite right
                         (round (chipmunk:y phys-pos)))))))))))

(defmethod run-system ((system render-system) found-components)
  (destructuring-bind (render-comp transform-comp) found-components
    (with-accessors ((sprite sprite) (flip flip)) render-comp
      (with-accessors ((local-pos position) (parent parent)) transform-comp
        (let* ((global-pos (local-to-global-position parent local-pos))
               (camera-obj (find-with-component (current-app-state) 'camera-tag))
               (camera-tr (find-component camera-obj 'transform-c))
               (camera-pos (position camera-tr)))
          (when sprite (draw-sprite sprite
                                    (- (car global-pos)
                                       (car camera-pos))
                                    (- (cdr global-pos)
                                       (cdr camera-pos)) :flip flip)))))))

(defmethod run-system ((system camera-system) found-components)
  (destructuring-bind (tr) found-components
    (let* ((tracked-obj (find-with-component (current-app-state) 'player-tag))
           (tracked-obj-tr (find-component tracked-obj 'transform-c))
           (tracked-obj-pos (position tracked-obj-tr)))
      (multiple-value-bind (screen-w screen-h) (sdl2:get-renderer-output-size
                                                (sdl2-renderer *renderer*))
        ;; Update the camera position to keep the player in the center
        (setf (position tr) (cons
                             (- (car tracked-obj-pos) (floor screen-w 2))
                             (- (cdr tracked-obj-pos) (floor screen-h 2))))))))

(defclass player-controlable-system (system)
  ((requires :initform '(player-controlable-c physical-c render-c shooter-c))))

(defmethod run-system ((system player-controlable-system) found-components)
  (destructuring-bind (player-controlable-comp physical render-comp shooter-comp) found-components
    (with-accessors ((max-move-speed max-move-speed)
                     (jmp-added-vel jumping-added-velocity)
                     (jumping? jumping?))
        player-controlable-comp
      (with-accessors ((body rigid-body)) physical
        (with-accessors ((flip flip)) render-comp
          (let* ((vel (chipmunk:velocity body)))
            (when (or (sdl2:keyboard-state-p :scancode-right)
                      (sdl2:keyboard-state-p :scancode-d))
              (setf (chipmunk:x vel) max-move-speed)
              (setf flip :none))
            (when (or (sdl2:keyboard-state-p :scancode-left)
                      (sdl2:keyboard-state-p :scancode-a))
              (setf (chipmunk:x vel) (- max-move-speed))
              (setf flip :horizontal))
            (when (and (or (sdl2:keyboard-state-p :scancode-up)
                           (sdl2:keyboard-state-p :scancode-space))
                       (not jumping?))
              (setf (chipmunk:y vel) jmp-added-vel)
              (setf jumping? t))
            (when (sdl2:keyboard-state-p :scancode-return)
              (with-accessors ((shoot? shoot?)) shooter-comp
                (setf shoot? t)))
            (when (sdl2:keyboard-state-p :scancode-r)
              (with-accessors ((shoot? shoot?)
                               (reload? reload?)) shooter-comp
                (unless shoot?
                  (setf reload? t))))

            (setf (chipmunk:velocity body) vel)))))))
