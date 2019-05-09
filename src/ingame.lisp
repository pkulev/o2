(in-package :o2)

(defclass ingame-state (state)
  ((running :initform nil
            :accessor running?)
   (space)))

(defmethod init ((ingame ingame-state))
  (with-slots (actor running space) ingame
    (setf running t)

    (let ((new-space (chipmunk:make-space))
          ;; Y is >0 because in SDL Y goes from top to bottom
          (gravity (chipmunk:make-cp-vect 0d0 (* 9.81d0 100))))
      (setf (chipmunk:gravity new-space) gravity)
      (setf space new-space))

    (chipmunk:clear-collision-types)
    (chipmunk:register-collision-type :james)
    (chipmunk:register-collision-type :ground)

    ;; FIXME: a better place for this?
    (let ((ground (chipmunk:make-segment-shape (chipmunk:body space)
                                               (chipmunk:make-cp-vect 0d0 550d0)
                                               (chipmunk:make-cp-vect 1280d0 550d0)
                                               0d0)))
      (setf (chipmunk:collision-type ground) :ground)
      (setf (chipmunk:friction ground) 1d0)
      (chipmunk:add space ground))

    ;; player
    (setf actor (make-james
                 :position '(50 . 400)
                 :sprite :player
                 :sitting-sprite :player-sitting
                 :space space))
    (add-child
     actor
     (add-object ingame
                 (make-instance 'G17
                                :components (list
                                             (make-instance 'transform-c))
                                :ammo 32
                                :current-ammo 17
                                :sprite :G17)))
    (select-weapon actor 'G17)

    (add-object ingame
                (make-instance 'background
                               :components (list
                                            (make-instance 'transform-c
                                                           :position (cons -512 -385))
                                            (make-instance 'render-c
                                                           :sprite :background
                                                           :render-priority 1))
                               :systems (list
                                         (make-instance 'render-system))))

    (add-object ingame actor)

    ;; bad guy
    ;; (add-object ingame (make-instance 'enemy-spawner))

    (add-object ingame (make-instance
                        'game-object
                        :components (list
                                     (make-instance 'transform-c)
                                     (make-instance 'render-c
                                                    :render-priority 2)
                                     (make-instance 'text-widget-c
                                                    :data-getter
                                                    (lambda ()
                                                      (format nil "Score: ~A"
                                                              (score (current-app-state))))))
                        :systems (list
                                  (make-instance 'text-widget-system))))


    (add-object ingame (make-instance
                        'game-object
                        :components (list
                                     (make-instance 'transform-c)
                                     (make-instance 'camera-tag))
                        :systems (list
                                  (make-instance 'camera-system))))

    ;;(add-object ingame (make-instance 'text-widget
    ;;                                  :x 0 :y 0
    ;;                                  :data-getter
    ;;                                  #'))
    ))

(defmethod cleanup ((state ingame-state))
  (with-slots (space) state
    (chipmunk:free-space space))
  (when (next-method-p) (call-next-method)))

(defmethod update ((state ingame-state) &key dt &allow-other-keys)
  (declare (ignorable dt))

  (with-slots (space) state
    ;; Chipmunk counts time in seconds, SDL - in milliseconds
    (chipmunk:step space (coerce (/ +delay+ 1000) 'double-float)))

  (when (next-method-p) (call-next-method)))

(defmethod render ((ingame ingame-state))
  (with-slots (score objects) ingame
    (dolist (object objects)
      ;(render object)
      )))

(defmethod process-input ((ingame ingame-state) direction keysym)
  (if (eq direction :keydown)
      (ingame-keydown ingame keysym)
      (ingame-keyup ingame keysym)))

(defun ingame-keyup (ingame keysym)
  (with-slots (actor) ingame
    (let ((scancode (sdl2:scancode-value keysym)))
      (when (or (sdl2:scancode= scancode :scancode-left) (sdl2:scancode= scancode :scancode-a))
        ;; (setf (x-accel actor) 0)
        ;; (setf (x-move-direction actor) 0)
        )

      (when (or (sdl2:scancode= scancode :scancode-right) (sdl2:scancode= scancode :scancode-d))
        ;; (setf (x-accel actor) 0)
        ;; (setf (x-move-direction actor) 0)
        )

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
        ;; (move-left actor)
        )
      (when (or (sdl2:scancode= scancode :scancode-right) (sdl2:scancode= scancode :scancode-d))
        ;; (move-right actor)
        )
      (when (or (sdl2:scancode= scancode :scancode-space) (sdl2:scancode= scancode :scancode-w))
        ;;(jump actor)
        )
      (when (or (sdl2:scancode= scancode :scancode-down) (sdl2:scancode= scancode :scancode-s))
        (setf (sitting? actor) t))
      (when (sdl2:scancode= scancode :scancode-return)
        (fire actor)))))
