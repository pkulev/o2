(in-package :o2)

(defclass ingame-state (state)
  ((running :initform nil
            :accessor running?)))

(defmethod init ((ingame ingame-state))
  (with-slots (actor running) ingame
    (setf running t)

    ;; player
    (setf actor (make-james
                 :x 50 :y 400
                 :sprite :player
                 :sitting-sprite :player-sitting))

    (add-child actor (add-object ingame
                                 (make-instance 'G17
                                                :ammo 32
                                                :current-ammo 17)))
    (select-weapon actor 'G17)

    (add-object ingame
                (make-instance 'background
                               :x -512 :y -385
                               :sprite :background))

    (add-object ingame actor)

    ;; bad guy
    (add-object ingame (make-instance 'enemy-spawner))

    (add-object ingame (make-instance 'text-widget
                                      :x 0 :y 0
                                      :data-getter
                                      #'(lambda ()
                                          (format nil "Score: ~A"
                                                  (score (current-app-state))))))))

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
        (setf (x-accel actor) 0)
        (setf (x-move-direction actor) 0))

      (when (or (sdl2:scancode= scancode :scancode-right) (sdl2:scancode= scancode :scancode-d))
        (setf (x-accel actor) 0)
        (setf (x-move-direction actor) 0))

      (when (or (sdl2:scancode= scancode :scancode-down) (sdl2:scancode= scancode :scancode-s))
        (setf (sitting? actor) nil)))))

(defun ingame-keydown (ingame keysym)
  (with-slots (actor running application) ingame
    (let ((scancode (sdl2:scancode-value keysym)))
      ;; TODO: replace with pause menu
      (when (sdl2:scancode= scancode :scancode-escape)
        (setf running nil)
        (set-state application :main-menu))

      (when (or (sdl2:scancode= scancode :scancode-left) (sdl2:scancode= scancode :scancode-a))
        (move-left actor))
      (when (or (sdl2:scancode= scancode :scancode-right) (sdl2:scancode= scancode :scancode-d))
        (move-right actor))
      (when (or (sdl2:scancode= scancode :scancode-space) (sdl2:scancode= scancode :scancode-w))
        (jump actor))
      (when (or (sdl2:scancode= scancode :scancode-down) (sdl2:scancode= scancode :scancode-s))
        (setf (sitting? actor) t))
      (when (sdl2:scancode= scancode :scancode-return)
        (fire actor)))))
