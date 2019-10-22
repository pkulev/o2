#| State representation for application.

State keeps a list of it's game objects, other objects should use weakrefs to
such objects.

In the future state must be fully loadable from it's corresponding file. So
state keeps objects (and it's preferences: arguments to pass to constructor?).
|#

(in-package :o2/engine)

(defclass state ()
  ((application :initarg :application
                :documentation "Application object.")  ; TODO: is it needed?
   (running :initarg :running
            :accessor running?  ; TODO: do we use it?
            :documentation "Whether is state currently running.")
   (actor :initarg :actor
          :accessor actor
          :documentation "Actor is the object that takes input from user.")

   (objects :initarg :objects
            :reader objects
            :documentation "List of all game objects for current state.")
   (systems :initarg :systems
            :accessor systems
            :documentation "Instances of the systems that are used in the current state.")

   (camera :initarg :camera
           :accessor camera ; TODO: current camera?
           :documentation "Camera position.")

   ;; FIXME: score must be game-side component attached to some GameController
   (score :initarg :score
          :accessor score
          :documentation "Current score.")

   ;; FIXME: physical space must not be part of the state
   (physical-space :initarg :physical-space
                   :accessor physical-space
                   :documentation "Physics-specific entity."))
  (:default-initargs
   :application (error "Application must be set.")
   :running nil
   :actor nil

   :objects (list)
   :systems (make-hash-table)

   :camera (cons 0 0)

   :score 0
   :physical-space nil))

(defgeneric init (state)
  (:documentation "Initialize state."))

(defmethod init ((state state)))

;; FIXME: unused method now
(defgeneric process-input (state direction keysym))

;; FIXME: unused method now
(defgeneric render (state))

(defgeneric update (state &key &allow-other-keys)
  (:documentation "Update all state objects."))

(defmethod update (state &key &allow-other-keys))


(defgeneric cleanup (what)
  (:documentation "Clean up all state's objects."))

(defmethod cleanup (what)) ;; A catch-all empty case

(defmethod cleanup ((state state))
  (with-slots (objects) state ;; Clean up all the objects
    (dolist (object objects)
      (with-accessors ((comps components)) object
        (dolist (comp comps) (cleanup comp))))))

(defgeneric add-object (state object)
  (:documentation "Add OBJECT to the STATE's list of objects."))

(defmethod add-object ((state state) (object game-object))
  (with-slots (objects) state
    (push object objects)

    ;; TODO: probably state shouldn't touch components and keep objects sorted,
    ;;       maybe list is not the best structure. Think about it someday.
    (setf objects (sort objects #'<
                        :key #'(lambda (it)
                                 (let ((render-c (find-component it 'render-c :raise-error nil)))
                                   ;; If there is a render component, then use it to order objects.
                                   ;; If there is not, just return zero, since this object does not care
                                   ;; about being rendered
                                   (if render-c
                                       (render-priority render-c)
                                       0)))))
    object))

(defgeneric remove-object (state object)
  (:documentation "Remove OBJECT from STATE's list of objects."))

(defmethod remove-object ((state state) (object game-object))
  (with-slots (objects) state
    (setf objects (remove object objects))))

(defun find-object-by-id (state id)
  "Return object of STATE by it's ID if exists, nil otherwise."
  (find-if (lambda (o) (= id (id o))) (objects state)))
