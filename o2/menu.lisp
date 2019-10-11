(in-package :o2)

(defclass main-menu-state (o2/engine:state)
  ((choice :initform 0
           :accessor choice)))

(defclass menu-choice-c (o2/engine:component)
  ((choice-number :initarg :choice-number
                  :accessor choice-number)
   (text :initarg :text
         :accessor text)
   (dimensions :initarg :dimensions
               :accessor dimensions)
   (color :initarg :color
          :accessor color
          :documentation "Color in the '(R G B A) format")
   (selected-color :initarg :selected-color
                   :accessor selected-color
                   :documentation "Color when selected, in the '(R G B A) format")
   (action :initarg :action
           :accessor action)))

(defclass menu-choice-system (o2/engine:system)
  ((requires :initform '(menu-choice-c o2/engine:transform-c))))

(defmethod o2/engine:run-system ((system menu-choice-system) found-components)
  (destructuring-bind (choice tr) found-components
    (with-slots ((current-choice choice)) (o2/engine:current-app-state)
      (with-accessors ((pos o2/engine:position)) tr
        (with-accessors ((button-choice choice-number) (dims dimensions)
                         (color color) (sel-color selected-color)
                         (text text))
            choice
          (o2/engine:draw-rect pos dims
                               (if (= current-choice button-choice)
                                   sel-color
                                   color))
          (o2/engine:draw-text :ubuntu-large text (car pos) (cdr pos)
                               :color '(255 255 255)))))))

(defclass menu-system (o2/engine:system)
  ((requires :initform '(o2/engine:transform-c))))

(defmethod o2/engine:run-system ((system menu-system) found-components)
  (destructuring-bind (tr) found-components
    (with-accessors ((children o2/engine:children)) tr
      (cond
        ((sdl2:keyboard-state-p :scancode-escape)
         (sdl2:push-event :quit))
        ((sdl2:keyboard-state-p :scancode-return)
         (loop named buttons
               for butt in children
               do (let ((butt-c (o2/engine:find-component butt 'menu-choice-c)))
                    (when (= (choice-number butt-c)
                             (choice (o2/engine:current-app-state)))
                      (funcall (action butt-c))
                      (return-from buttons)))))
        ;; FIXME: BIG HACK FOR TWO BUTTONS ONLY, FIX
        ;; FIXME: add a key-repeat delay
        ((sdl2:keyboard-state-p :scancode-down)
         (with-slots (choice) (o2/engine:current-app-state)
           (setf choice (mod (1+ choice) 2))))
        ((sdl2:keyboard-state-p :scancode-up)
         (with-slots (choice) (o2/engine:current-app-state)
           (setf choice (mod (1- choice) 2))))))))

(defmethod o2/engine:init ((menu main-menu-state))
  ;; A static camera
  (o2/engine:add-object
   menu
   (o2/engine:make-game-object :components (o2/engine:transform-c
                                            o2/engine:camera-tag)))

  ;; The background image
  (o2/engine:add-object
   menu
   (o2/engine:make-game-object
    :components
    ((o2/engine:transform-c :position '(-512 . -385))
     (o2/engine:render-c :sprite :background
                         :render-priority 0))
    :systems
    (o2/engine:render-system)))

  (o2/engine:add-object
   menu
   (o2/engine:make-game-object
    :components
    ((o2/engine:render-c :sprite :logo
                         :render-priority 1)
     (o2/engine:transform-c :position '(150 . 100)))
    :systems (o2/engine:render-system)))

  (let* ((menu-object
           (o2/engine:add-object
            menu
            (o2/engine:make-game-object :components (o2/engine:transform-c)
                                        :systems (menu-system))))
         (button-w 500)
         (button-h 100)
         (base-butt-x (- (/ 1024 2) (/ button-w 2)))
         (base-butt-y (+ (/ 768 2) (/ button-h 2))))

    (o2/engine:add-child
     menu-object
     (o2/engine:add-object
      menu
      (o2/engine:make-game-object
       :components ((o2/engine:transform-c :position (cons base-butt-x base-butt-y))
                    (o2/engine:render-c :render-priority 1)
                    (menu-choice-c
                     :color '(29 149 182 128)
                     :selected-color '(17 95 118 192)
                     :dimensions (cons button-w button-h)
                     :choice-number 0
                     :text "New game"
                     :action (lambda ()
                               (o2/engine:push-event :change-current-state 'ingame-state))))
       :systems (menu-choice-system))))
    (o2/engine:add-child
     menu-object
     (o2/engine:add-object
      menu
      (o2/engine:make-game-object
       :components ((o2/engine:transform-c :position (cons base-butt-x
                                                           (+ base-butt-y button-h 25)))
                    (o2/engine:render-c :render-priority 1)
                    (menu-choice-c
                     :color '(29 149 182 128)
                     :selected-color '(17 95 118 192)
                     :dimensions (cons button-w button-h)
                     :choice-number 1
                     :text "Exit"
                     :action #'sdl2:push-quit-event))
       :systems (menu-choice-system))))))
