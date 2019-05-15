(in-package :o2)

(defclass state ()
  ((application :initform (error "Application must be set.")
                :initarg :application)
   (running :initform nil
            :accessor running?)
   (actor :initform nil
          :accessor actor)
   (objects :initform (list)
            :reader objects)
   (camera :initform (cons 0 0)
           :accessor camera)

   ;; FIXME: is this really the best place for it?
   (score :initform 0
          :accessor score)))

(defgeneric init (state))
(defgeneric process-input (state direction keysym))
(defgeneric update (state &key &allow-other-keys))
(defgeneric render (state))
(defgeneric add-object (state object))

(defmethod update (state &key &allow-other-keys))

(defmethod init ((state state)))

(defgeneric cleanup (what))
(defmethod cleanup (what)) ;; A catch-all empty case

(defmethod cleanup ((state state))
  (with-slots (objects) state ;; Clean up all the objects
    (dolist (object objects)
      (with-accessors ((comps components)) object
        (dolist (comp comps) (cleanup comp))))))

(defmethod add-object ((state state) (object game-object))
  (with-slots (objects) state
    (push object objects)

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

(defmethod remove-object ((state state) (object game-object))
  (with-slots (objects) state
    (setf objects (remove object objects))))

(defun find-object-by-id (state id)
  (find-if (lambda (o) (= id (id o))) (objects state)))
