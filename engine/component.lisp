(in-package :o2/engine)

;; TODO: fix this
;; Component cleanups, which will be done after the physics step
(defparameter *physical-component-cleanups* '())

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

(defclass shooter-c (component)
  ((shoot? :accessor shoot?
           :initform nil)
   (reload? :accessor reload?
            :initform nil)
   (weapons :accessor weapons
            :initarg :weapons)
   (current-weapon :accessor current-weapon
                   :initarg :current-weapon)
   (bullet-collision-type :accessor bullet-collision-type
                          :initarg :bullet-collision-type
                          :documentation "What collision type should be used for callbacks")
   (bullet-shape-filter :accessor bullet-shape-filter
                        :initarg :bullet-shape-filter
                        :documentation "What category should be used for collision checks"))
  (:default-initargs :weapons (list)))

(defclass player-tag (component) ())
(defclass camera-tag (component) ())

(defun find-component (game-object component-name &key (raise-error t))
  "Tries to find the component in the game-object, and if it could not be found,
   raises a error, unless raise-error is nil"
  (with-accessors ((components components)) game-object
    (let ((maybe-found-component
            ;; Try to find the named component
            (find-if
             (lambda (component)
               (equal (class-name (class-of component)) component-name))
             components)))
      (when (and (not maybe-found-component) raise-error)
        (error "~A component was not found in ~A" component-name game-object))
      ;; If the component was found, return it, if it was not, an error was
      ;; signaled already
      maybe-found-component)))

(defun find-with-component (state component-name)
  "Find the first object with the specified component."
  (with-accessors ((objects objects)) state
    (find-if
     (lambda (obj)
       (find-component obj component-name :raise-error nil))
     objects)))

(defmethod cleanup ((comp physical-c))
  ;; Delay cleanup to the time after the physics step
  (push
   (lambda ()
     (with-slots (physical-space) (current-app-state)
       (when physical-space
         (with-accessors ((body rigid-body) (shape shape)) comp
           (chipmunk:remove physical-space body)
           (chipmunk:remove physical-space shape)

           (chipmunk:free-shape shape)
           (chipmunk:free-body body)))))
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


(defmethod global-position ((tr transform-c))
  (with-accessors ((pos position) (parent parent)) tr
    ;; If there is a parent, it'll be noted and translated into a global position
    ;; If not, it'll just return the global position
    (local-to-global-position parent pos)))

(defmethod global-position ((object game-object))
  "Gets the global/world position of the object"
  (let ((tr (find-component object 'transform-c)))
    (global-position tr)))
