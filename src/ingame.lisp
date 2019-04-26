(in-package :o2)

(defclass ingame-state (state)
  ((running :initform nil
            :accessor running?)
   (objects :initform (list)
            :reader objects)))

(defmethod init ((ingame ingame-state))
  (with-slots (actor running objects) ingame
    (setf running t)

    ;; player
    (setf actor (make-instance 'james
                               :x 50 :y 400
                               :sprite :player))

    (setf objects (list
                   ;; just something invisible
                   (make-instance 'game-object
                                  :x 10 :y 10
                                  :sprite nil)

                   ;; background
                   (make-instance 'game-object
                                  :sprite :background)

                   ;; TODO: implement sorting by render-priority
                   actor

                   ;; bad guy
                   (make-instance 'game-object
                                  :x 700 :y 400
                                  :sprite :bad-guy)))))

(defmethod update ((ingame ingame-state) &key dt &allow-other-keys)
  (with-slots (running objects) ingame
    (when running
      (dolist (object objects)
        (update object)))))

(defmethod render ((ingame ingame-state))
  (with-slots (score objects) ingame
    (dolist (object objects)
      (render object))))

(defmethod process-input ((ingame ingame-state) direction keysym)
  (if (eq direction :keydown)
      (ingame-keydown ingame keysym)
      (ingame-keyup ingame keysym)))

(defun ingame-keyup (ingame keysym)
  (with-slots (actor) ingame
    (let ((scancode (sdl2:scancode-value keysym)))
      (when (or (sdl2:scancode= scancode :scancode-left) (sdl2:scancode= scancode :scancode-a))
        (setf (ax actor) 0)
        (setf (move-direction-h actor) 0))

      (when (or (sdl2:scancode= scancode :scancode-right) (sdl2:scancode= scancode :scancode-d))
        (setf (ax actor) 0)
        (setf (move-direction-h actor) 0)))))

(defun ingame-keydown (ingame keysym)
  (with-slots (actor running application) ingame
    (let ((scancode (sdl2:scancode-value keysym)))
      ;; TODO: pause
      (when (sdl2:scancode= scancode :scancode-escape)
        nil)

      (when (or (sdl2:scancode= scancode :scancode-left) (sdl2:scancode= scancode :scancode-a))
        (move-left actor))
      (when (or (sdl2:scancode= scancode :scancode-right) (sdl2:scancode= scancode :scancode-d))
        (move-right actor))
      (when (sdl2:scancode= scancode :scancode-space)
        (jump actor))
      (when (sdl2:scancode= scancode :scancode-return)
        (fire actor)))))
