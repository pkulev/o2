(in-package :o2)

(defclass state ()
  ((name :initform (error "Name must be set") ;; TODO drop
         :initarg :name)
   (application :initform (error "Application must be set")
                :initarg :application)
   (renderer :initform (error "Renderer must be set")
             :initarg :renderer)
   (actor :initform nil
          :accessor actor)))

(defgeneric init (state))
(defgeneric process-input (state direction keysym))
(defgeneric update (state &key &allow-other-keys))
(defgeneric render (state))
(defgeneric cleanup (state))

(defmethod init ((state state)))
(defmethod update ((state state) &key dt &allow-other-keys))
