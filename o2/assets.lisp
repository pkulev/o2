#| Asset representation.

All external resources (e.g. images, models, sounds, etc.) represented with
`asset' objects within the engine.
|#

(in-package :o2/engine)


(defclass asset ()
  ((*assets* :initform nil
             :allocation :class
             :documentation "all loaded assets")
   (name :initarg :name
         :reader name
         :documentation "asset name")
   (path :initarg :path
         :reader path
         :documentation "asset path"))
  (:default-initargs
   :name (error "Name must be set.")
   :path nil))

(defmethod initialize-instance :after ((obj asset) &key &allow-other-keys)
  "Store initialized asset in *assets*."
  (setf (slot-value obj '*assets*) (append (slot-value obj '*assets*) obj)))

;; TODO: convinient asset definition
;;(defmacro make-asset (type &args other))


(defclass sprite (asset)
  ((texture :initarg :texture
            :accessor texture
            :documentation "sprite texture object")
   (blend-mode :initarg :blend-mode
               :accessor blend-mode
               :documentation "sprite blend mode"))
  (:default-initargs
   :texture (error "Texture must be set.")
   :blend-mode :none))


(defclass font (asset)
  ((font-data :initarg font-data
              :accessor font-data
              :documentation "inner font data"))
  (:default-initargs
   :font-data (error "Font data must be set.")))
