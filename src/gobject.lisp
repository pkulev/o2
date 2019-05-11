(in-package :o2)

(defclass transform ()
  ((x :initform 0
      :initarg :x
      :accessor x)
   (y :initform 0
      :initarg :y
      :accessor y)))

(defparameter *game-object-next-id* 0)

(defclass game-object ()
  ((id :initform (incf *game-object-next-id*)
       :reader id)
   (components :initform (list)
               :initarg :components
               :reader components)
   (systems :initform (list)
            :initarg :systems
            :reader systems)

   (render-priority :initform 0
                    :accessor render-priority
                    :allocation :class)
   (sprite :initform nil
           :initarg :sprite
           :reader sprite)

   (parent :initform nil
           :reader parent)

   (children :initform (list)
             :reader children)))

(defmethod initialize-instance :after ((obj game-object) &key)
  ;; Set all the "game-object" slots to this object
  (dolist (system (systems obj))
    (with-slots (game-object) system
      (setf game-object obj))))

(defgeneric update (object &key &allow-other-keys)
  (:documentation "Update game object state."))

(defmethod update ((object game-object) &key dt &allow-other-keys)
  (declare (ignorable dt))

  (run-systems object)

  (with-slots (children) object
    (dolist (child children)
      (update child))))

(defmethod render ((object game-object))
  (with-slots (x y sprite children) object
    (with-slots (camera) (current-app-state)
      (when sprite (draw-sprite sprite (- x (car camera)) (- y (cdr camera)))))
    (dolist (child children) (render child))))

(defmethod get-rect ((object game-object))
  (with-slots (x y sprite) object
    (if sprite
        (progn
          (let* ((texture (get-sprite-texture sprite))
                 (w (sdl2:texture-width texture))
                 (h (sdl2:texture-height texture)))
            (sdl2:make-rect x y w h)))
        (sdl2:make-rect x y 0 0))))

(defmethod add-child ((object game-object) (child game-object))
  (let ((tr (find-component object 'transform-c))
        (child-tr (find-component child 'transform-c)))
    (with-accessors ((children children)) tr
      (with-accessors ((parent parent)) child-tr
        (unless (find child children)
          (push child children)
          (setf parent object)))))
  child)

(defmethod remove-child ((object game-object) (child game-object))
  (let ((tr (find-component object 'transform-c))
        (child-tr (find-component child 'transform-c)))
    (with-accessors ((children children)) tr
      (with-accessors ((parent parent)) child-tr
        (setf children (remove child children))
        (setf parent nil)))))

(defmethod destroy ((obj game-object))
  (let ((tr (find-component obj 'transform-c)))
    (when tr
      ;; If there is a transform component, also destroy any children that might exist
      (with-accessors ((parent parent) (children children)) tr
        (dolist (child children) (destroy child))

        ;; Disconnect from parent, if there is one.
        ;; This also removes the parent reference from the child
        (when parent
          (with-slots (children) parent
            (remove-child parent obj))))))

  ;; Remove the object from the global object list
  (remove-object (current-app-state) obj)

  ;; Finally, clean up all the components. After that the object is probably unusable
  (dolist (comp (components obj)) (cleanup comp)))
