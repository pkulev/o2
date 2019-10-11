(in-package :o2/engine)

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
  "Set RENDERER as current."
  (log:debug "setting" renderer "as current renderer")
  (setf *renderer* renderer))

(defun load-texture (renderer path-to-file)
  (let ((surface (sdl2-image:load-image path-to-file)))
    (sdl2:set-color-key surface
                        :true (sdl2:map-rgb (sdl2:surface-format surface)
                                            255 0 255))
    (sdl2:create-texture-from-surface renderer surface)))

(defun load-font (path-to-file &key (font-size 16))
  (sdl2-ttf:open-font path-to-file font-size))

(defun add-sprite (sprite-name path-to-file &key (blend-mode :none))
  (check-type sprite-name keyword)

  (with-slots (sdl2-renderer sprites) *renderer*
    (if (assoc sprite-name sprites)
        (error "Sprite ~a already registered" sprite-name))
    (let ((texture (load-texture sdl2-renderer path-to-file)))
      (sdl2:set-texture-blend-mode texture blend-mode)
      (setf sprites (acons sprite-name texture sprites))
      sprite-name)))

(defun add-font (font-name path-to-file &key (font-size 16))
  (check-type font-name keyword)

  (with-slots (fonts) *renderer*
    (if (assoc font-name fonts)
        (error "Font ~a already registered" font-name))
    (let ((font (load-font path-to-file :font-size font-size)))      
      (setf fonts (acons font-name font fonts))
      font-name)))

(defun remove-sprite (sprite-name)
  (check-type sprite-name keyword)
  
  (with-slots (sprites) *renderer*
    (setf sprites (remove sprite-name sprites :key #'car))
    t))

(defun get-sprite-texture (sprite-name)
  (check-type sprite-name keyword)

  (cdr (assoc sprite-name
              (renderer-sprites *renderer*))))

(defun get-font (font-name)
  (check-type font-name keyword)

  (cdr (assoc font-name (renderer-fonts *renderer*))))

(defun sprite-width (sprite-name)
  (sdl2:texture-width (get-sprite-texture sprite-name)))

(defun sprite-height (sprite-name)
  (sdl2:texture-height (get-sprite-texture sprite-name)))

(defun draw-rect (position dimensions rgba-color &key (blendmode :blend) (fill t))
  (destructuring-bind (x . y) position
    (destructuring-bind (w . h) dimensions
      (destructuring-bind (r g b a) rgba-color
        (with-slots (sdl2-renderer) *renderer*
          (sdl2:set-render-draw-blend-mode sdl2-renderer blendmode)
          (sdl2:set-render-draw-color sdl2-renderer r g b a)
          (if fill
              (sdl2:render-fill-rect sdl2-renderer (sdl2:make-rect x y w h))
              (sdl2:render-draw-rect sdl2-renderer (sdl2:make-rect x y w h))))))))

(defun draw-sprite (sprite-name x y &key (blendmode :blend) (scale 1.0) flip)
  (let* ((texture (get-sprite-texture sprite-name))
         (sdl2-renderer (slot-value *renderer* 'sdl2-renderer))
         (w (round (* scale (sdl2:texture-width texture))))
         (h (round (* scale (sdl2:texture-height texture))))
         (dest-rect (sdl2:make-rect x y w h)))
    (sdl2:set-texture-blend-mode texture blendmode)    
    (if flip (sdl2:render-copy-ex sdl2-renderer texture :dest-rect dest-rect :flip (list flip))
        (sdl2:render-copy sdl2-renderer texture :dest-rect dest-rect))))

(defun draw-text (font-name text x y &key (color '(0 0 0)) (alpha 0))
  (destructuring-bind (r g b) color
    (with-slots (sdl2-renderer) *renderer*
      (let ((font (get-font font-name)))
        (let* ((surface (sdl2-ttf:render-text-solid font text r g b alpha))
               (texture (sdl2:create-texture-from-surface sdl2-renderer surface))
               (dest-rect (sdl2:make-rect x y (sdl2:texture-width texture) (sdl2:texture-height texture))))
          (sdl2:render-copy sdl2-renderer texture :dest-rect dest-rect))))))
