(in-package :o2)

(defclass component () ())

(defclass transform-c (component)
  ((position :initarg :position
             :accessor position)
   (parent :initarg :parent
           :accessor parent)
   (children :initarg :children
             :accessor children))
  (:default-initargs :position '(0 . 0)
                     :parent nil
                     :children (list)))

(defclass physical-c (component)
  ((rigid-body :initarg :rigid-body
               :accessor rigid-body)
   (shape :initarg :shape
          :accessor shape)))

(defclass render-c (component)
  ((render-priority :initarg :render-priority
                    :accessor render-priority)
   (sprite :initarg :sprite
           :accessor sprite)
   (flip :initarg :flip
         :accessor flip))
  (:default-initargs :render-priority 0 :sprite nil
                     :flip :none))

(defclass player-controlable-c (component)
  ((max-move-speed :initarg :max-move-speed
                   :accessor max-move-speed
                   :documentation "Maximum player move speed")
   (jumping-added-velocity :initarg :jumping-added-velocity
                           :accessor jumping-added-velocity)
   (jumping? :initform nil
             :accessor jumping?))
  (:default-initargs :max-move-speed 200d0
                     :jumping-added-velocity -200d0))

(defclass player-tag (component) ())
(defclass camera-tag (component) ())

(defclass system ()
  ((requires :initform '()
             :accessor requires)
   (game-object :reader game-object)))

(defclass physical-system (system)
  ((requires :initform '(transform-c physical-c))))

(defclass player-controlable-system (system)
  ((requires :initform '(player-controlable-c physical-c render-c shooter-c))))

(defclass render-system (system)
  ((requires :initform '(render-c transform-c))))

(defclass camera-system (system)
  ((requires :initform '(transform-c))))

(defun find-component (game-object component-name &key (raise-error t))
  "Tries to find the component in the game-object, and if it could not be found,
   raises a error, unless raise-error is nil"
  (with-accessors ((components components)) game-object
    (let ((maybe-found-component
            ;; Try to find the named component
            (find-if
             (lambda (component) (equal (class-name (class-of component)) component-name))
             components)))
      (when (and (not maybe-found-component) raise-error)
        (error "~A component was not found in ~A" component-name game-object))
      ;; If the component was found, return it, if it was not, an error was signaled already
      maybe-found-component)))

(defun find-with-component (state component-name)
  "Find the first object with the specified component or returns nil if there weren't any."
  (with-accessors ((objects objects)) state
    (find-if
     (lambda (obj)
       (find-component obj component-name :raise-error nil))
     objects)))

(defmethod run-systems (object)
  (with-accessors ((systems systems) (components components)) object
    (dolist (system systems)
      (with-accessors ((requirements requires)) system
        ;; Find the required components that the system specified,
        ;; if some of them were not found, an error will be signaled by the
        ;; find-component function
        ;; TODO: have a way to handle not finding components, like a special signal or something
        (let ((found-required-components
                (loop for required-comp-name in requirements
                      collect (find-component object required-comp-name))))
          (run-system system found-required-components))))))


(defgeneric run-system (system found-required-components))

(defmethod run-system ((system physical-system) found-components)
  "Update the position of the object from the physical system"
  (destructuring-bind (transform physical) found-components
    (with-accessors ((body rigid-body)) physical
      (with-accessors ((pos position) (parent parent)) transform
        (let ((phys-pos (chipmunk:position body)))
          (setf pos
                ;; Translate the global position from the physics system into a local position,
                ;; if there is a parent.
                (global-to-local-position
                 parent
                 (cons (round (chipmunk:x phys-pos))
                       (round (chipmunk:y phys-pos))))))))))

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

            (setf (chipmunk:velocity body) vel)))))))

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

;; Component cleanups, which will be done after the physics step
(defparameter *physical-component-cleanups* '())

(defmethod cleanup ((comp physical-c))
  ;; Delay cleanup to the time after the physics step
  (push
   (lambda ()
     (with-accessors ((body rigid-body) (shape shape)) comp
       (with-slots (space) (current-app-state)
         (chipmunk:remove space body)
         (chipmunk:remove space shape))

       (chipmunk:free-shape shape)
       (chipmunk:free-body body)))
   *physical-component-cleanups*))

(defun local-to-global-position (parent l-position)
  "Translates a local position relatively to the provided parent.
   If parent is nil, local position == global position"
  (if parent
      ;; If there is a parent, translate local position to a global one
      (let ((parent-global-pos (global-position parent)))
        (cons
         (+ (car parent-global-pos) (car l-position))
         (+ (cdr parent-global-pos) (cdr l-position))))
      ;; Otherwise, local position == global position
      l-position))

(defun global-to-local-position (parent g-position)
  "Translates a global position to a local position relatively to the provided parent.
   If parent is nil, global position == local position"
  (if parent
      ;; If there's a parent, translate position
      (let* ((parent-global-pos (global-position parent)))
        (cons
         (- (car g-position) (car parent-global-pos))
         (- (cdr g-position) (cdr parent-global-pos))))
      ;; If not, they're the same position
      g-position))

(defun global-position (object)
  "Gets the global/world position of the object"
  (let ((tr (find-component object 'transform-c)))
    (with-accessors ((pos position) (parent parent)) tr
      ;; If there is a parent, it'll be noted and translated into a global position
      ;; If not, it'll just return the global position
      (local-to-global-position parent pos))))
