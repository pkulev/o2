(in-package :o2/engine)

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

(defmacro make-game-object (&key components systems)
  "Constructs a new game object with thr specified components and systems.

   The component/system (just component from here on) can be of two forms:
   (component-name arguments...), which will create a component of class component-name with the provided
   arguments passed to make-instance; and component-name, without any arguments, which will create the component
   with make-instance and not pass any arguments to it (e.g. systems don't need arguments,
   so it's very handy that way)

   Example usage: (make-game-object :components (component-1 (component-2 arg-1))
                                    :systems (system-1 system-2))
  "
  (flet ((lists-to-makes (lst)
           (loop for data in lst
                 collect (etypecase data
                           (list
                            (destructuring-bind (class-name &rest args) data
                              `(make-instance ',class-name ,@args)))
                           (symbol `(make-instance ',data))))))
    `(make-instance 'game-object
                    :components (list ,@(lists-to-makes components))
                    :systems ',systems)))

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
