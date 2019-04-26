(in-package :o2)

(defclass renderer ()
  ((sdl2-renderer :initform (error "Renderer must be set")
                  :initarg :renderer
                  :accessor sdl2-renderer)
   (sprites :initform (list)
            :accessor renderer-sprites)
   (fonts :initform (list)
          :accessor renderer-fonts)))

(defvar *renderer* nil "Current renderer.")

(defun set-current-renderer (renderer)
  (setf *renderer* renderer))

(defun load-texture (renderer path-to-file)
  (sdl2:create-texture-from-surface
   renderer (sdl2-image:load-image path-to-file)))

(defun add-sprite (sprite-name path-to-file &key (blend-mode :none))
  (etypecase sprite-name
    (keyword
     (with-slots (sdl2-renderer sprites) *renderer*
       (if (assoc sprite-name sprites)
           (error "Sprite with name ~a already registered" sprite-name))
       (let ((texture (load-texture sdl2-renderer path-to-file)))
         (sdl2:set-texture-blend-mode texture blend-mode)
         (setf sprites (acons sprite-name texture sprites))
         sprite-name)))))

(defun remove-sprite (sprite-name)
  (etypecase sprite-name
    (keyword (with-slots (sprites) *renderer*
               (setf sprites (remove sprite-name sprites :key #'car))
               t))))

(defun get-sprite-texture (sprite-name)
  (etypecase sprite-name
    (keyword (cdr (assoc sprite-name
                         (renderer-sprites *renderer*))))))

(defun sprite-width (sprite-name)
  (sdl2:texture-width (get-sprite-texture sprite-name)))

(defun sprite-height (sprite-name)
  (sdl2:texture-height (get-sprite-texture sprite-name)))

(defun draw-rect (x y w h r g b a &key (blendmode :blend) (fill t))
  (with-slots (sdl2-renderer) *renderer*
    (sdl2:set-render-draw-blend-mode sdl2-renderer blendmode)
    (sdl2:set-render-draw-color sdl2-renderer r g b a)
    (if fill
        (sdl2:render-fill-rect sdl2-renderer (sdl2:make-rect x y w h))
        (sdl2:render-draw-rect sdl2-renderer (sdl2:make-rect x y w h)))))

(defun draw-sprite (sprite-name x y &key (blendmode :blend) (scale 1.0) flip)
  (let* ((texture (get-sprite-texture sprite-name))
         (sdl2-renderer (slot-value *renderer* 'sdl2-renderer))
         (w (round (* scale (sdl2:texture-width texture))))
         (h (round (* scale (sdl2:texture-height texture))))
         (dest-rect (sdl2:make-rect x y w h)))
    (sdl2:set-texture-blend-mode texture blendmode)
    (if flip (sdl2:render-copy-ex sdl2-renderer texture :dest-rect dest-rect :flip (list flip))
        (sdl2:render-copy sdl2-renderer texture :dest-rect dest-rect))))

