(in-package :o2)

(defclass menu-background (transform game-object)
  ((logo :initform :logo)))

(defun make-menu-background (&key x y)
  (let ((menu-bg (make-instance 'menu-background
                                :x x :y y
                                :sprite :background)))
    menu-bg))


(defclass main-menu-state (state)
  ((choise :initform 0)
   (background :initform nil)))

(defmethod init ((menu main-menu-state))
  (with-slots (background) menu
    (setf background (make-menu-background
                      :x -512 :y -385))))

(defmethod update ((menu main-menu-state) &key dt &allow-other-keys)
  (declare (ignorable dt)))

(defmethod render ((menu main-menu-state))
  (with-slots (choise background) menu
    (render background)
    ;;(draw-rect 0 0 1024 768 83 3 116 255)
    (let* ((button-w 500)
           (button-h 100)
           (button-x (- (/ 1024 2) (/ button-w 2)))
           (button-y (+ (/ 768 2) (/ button-h 2)))
           (button-color '(29 149 182 128))
           (button-selected-color '(17 95 118 255)))
      
      (if (= choise 0)
          (draw-rect button-x button-y button-w button-h
                     17 95 118 192)
          (draw-rect button-x button-y button-w button-h
                     29 149 182 128))
      (draw-text :ubuntu-large "New game" button-x button-y
                 :color '(255 255 255))

      (if (= choise 1)
          (draw-rect button-x (+ button-y button-h 25) button-w button-h
                     17 95 118 192)
          (draw-rect button-x (+ button-y button-h 25) button-w button-h
                     29 149 182 128))
      (draw-text :ubuntu-large "Exit" button-x (+ button-y button-h 25)
                 :color '(255 255 255)))))

(defmethod process-input ((menu main-menu-state) direction keysym)
  (if (eq direction :keydown)
      (main-menu-keydown menu keysym)
      (main-menu-keyup menu keysym)))

(defun main-menu-keyup (menu keysym)
  (let ((scancode (sdl2:scancode-value keysym)))
    (cond ((sdl2:scancode= scancode :scancode-escape)
           (sdl2:push-event :quit))
          ((sdl2:scancode= scancode :scancode-return)
           (let ((choise (slot-value menu 'choise)))
             (cond ((= choise 1)
                    (sdl2:push-event :quit))
                   ((= choise 0)
                    (with-slots (application) menu
                      (set-state application :ingame)))))))))

(defun main-menu-keydown (menu keysym)
  (let ((scancode (sdl2:scancode-value keysym)))
    (cond ((or (sdl2:scancode= scancode :scancode-down) (sdl2:scancode= scancode :scancode-s))
           (setf (slot-value menu 'choise)
                 (mod (1+ (slot-value menu 'choise))
                      2)))
          ((or (sdl2:scancode= scancode :scancode-up) (sdl2:scancode= scancode :scancode-w))
           (setf (slot-value menu 'choise)
                 (mod (1- (slot-value menu 'choise))
                      2))))))
