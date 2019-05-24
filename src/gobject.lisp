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
               :accessor components)
   (systems :initform (list)
            :initarg :systems
            :accessor systems)))

(defmethod initialize-instance :after ((obj game-object) &key)
  ;; Set all the "game-object" slots to this object
  (dolist (system (systems obj))
    (with-slots (game-object) system
      (setf game-object obj))))

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
